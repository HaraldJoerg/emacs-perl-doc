;;; perl-doc.el --- Read Perl documentation -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg <haj@posteo.de>
;; Created: 2022
;; Version: 0.81

;; Keywords: languages
;; URL: https://github.com/HaraldJoerg/emacs-perl-doc

;; Package-Requires: ((emacs "27"))

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains a command to read Perl documentation in Emacs.
;; It uses two external commands which come with Perl: `perldoc` to
;; locate the Perl documentation for the Perl modules installed on
;; your system, and `pod2html` to format the documentation to HTML.
;; This HTML version is then displayed using Emacs' "simple HTML
;; renderer" shr.
;;
;; Motivation
;;
;; Perl documentation is written in a markup format called POD ("Plain
;; Old Documentation") and is usually converted to other formats for
;; reading by humans.  The documentation used to be available in Emacs
;; for a long time in 'info' or 'man' format.  However, Perl does no
;; longer ship 'info' files, and the software available from CPAN
;; never did.  'man' is not available on all platforms and allows only
;; rather restricted formatting, most notably linking between
;; documents does not work.

;; On the other hand, Perl provides a converter from POD to HTML.
;; HTML is well supported by Emacs and is well suited for presentation
;; of structured documents.

;; The user visible benefits over the other formats are:
;;
;;  * Works nicely on platforms which do not have man
;;
;;  * Unlike with 'man', Hyperlinks between POD documents work and
;;   resolve to POD documentation on your system, no web server
;;   required.
;;
;;  * Makes use of Emacs faces: variable-pitch font for text,
;;    fixed-pitch for code, italics for, well, italics

;;; Code:

;;; Compatibility with older Emacs versions
;; Available in Emacs 28: format-prompt
(defalias 'perl-doc--format-prompt
  (if (fboundp 'format-prompt) 'format-prompt
    (lambda (msg default)
      (if default (format "%s (default %s): " msg default)
	(concat msg ": ")))))

;; Available in Emacs 28: string-search
(defun perl-doc--string-search (needle haystack)
  "Search for the string NEEDLE in the string HAYSTACK.
Returns nil if no NEEDLE was found, non-nil otherwise.  This is a
reduced version of the function `string-search' which is
sufficient for the purpose of this module and works in Emacs 27."
  (string-match (regexp-quote needle) haystack))

;; We use some features from cperl-mode:
;;  * cperl-word-at-point  : Finding Perl syntax elements
;;  * cperl-short-docs	   : Tell functions from modules (for use with -f)

(require 'cperl-mode)
(require 'shr)
(require 'face-remap)
(require 'imenu)
(require 'speedbar)

(defcustom perl-doc-perl-program "perl"
  "Path to the Perl interpreter."
  :type 'file
  :group 'perl-doc)

(defcustom perl-doc-pod2html-program "pod2html"
  "Path to the shell command pod2html."
  :type 'file
  :group 'perl-doc)

(defcustom perl-doc-perldoc-program "perldoc"
  "Path to the shell command perldoc."
  :type 'file
  :group 'perl-doc)

(defcustom perl-doc-extra-libs '()
  "A list of directories to scan for POD documentation.
This is in addition to what's built into the Perl executable as
@INC and what is in the environment variable \"PERL5LIB\".  This
allows to read documentation for modules which you do not want to
be used by the Perl interpreter."
  :group 'perl-doc
  :type '(repeat directory))

(defvar perl-doc--debug nil
  "If non-nil, unrecognized POD links are reported to the message buffer.
This is only relevant for developers, not for users.")

;; Make elint-current-buffer happy
(defvar button-buffer-map)		; in button.el
(defvar special-mode-map)		; in simple.el

(defun perl-doc--index-available-p ()
  "Return a non-nil value if an `imenu' index can be built.
Our current implementation is based on shr, which started to use
named faces for headings (which we use to build the index) in
Emacs 28."
  (facep 'shr-h1))

(defvar perl-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap button-buffer-map special-mode-map))
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] #'perl-doc-browse-url)
    (define-key map "\r" #'perl-doc-browse-url)
    (define-key map "v" #'perl-doc-view-source)
    map)
  "A keymap to allow following links in perldoc buffers.")

(defmacro perl-doc--with-environment-variables (variables &rest body)
  "Set VARIABLES in the environment and execute BODY.
VARIABLES is a list of variable settings of the form (VAR VALUE),
where VAR is the name of the variable (a string) and VALUE
is its value (also a string).

The previous values will be restored upon exit.

This is an 1:1 copy of the macro with-environment-variables.
I (haj) was about to write it a macro which did that when I
noticed that it is included in Emacs 28.  So I dropped my own
version, and will drop this one as well when the minimum Emacs
version is raised to 28."
  (declare (indent 1) (debug (sexp body)))
  (unless (consp variables)
    (error "Invalid VARIABLES: %s" variables))
  `(let ((process-environment (copy-sequence process-environment)))
     ,@(mapcar (lambda (elem)
                 `(setenv ,(car elem) ,(cadr elem)))
               variables)
     ,@body))

;;;
;;; perl-doc-mode Index functions
;;;

(defun perl-doc--heading-properties (_ face-or-list-of-faces)
  "Examine the FACE-OR-LIST-OF-FACES for headings.
If a heading face is present, return a cons cell (face . depth)
with the actual face found and its numerical depth in the hierarchy.
Return nil if no heading face is present in FACE-OR-LIST-OF-FACES."
  (let ((faces (if (listp face-or-list-of-faces)
		   face-or-list-of-faces
		 (list face-or-list-of-faces)))
	face level)
    (while (and faces (null level))
      (if (member (setq face (car faces))
		  '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6))
	  (setq level (string-to-number
		       (substring (face-name face) -1)))
	(setq faces (cdr faces))))
    (if level (cons face level) nil)))

(defun perl-doc--heading-face-end-p (expected got)
  "Find the first character where the face EXPECTED is not in GOT."
  (not (member expected (if (listp got) got (list got)))))

(defun perl-doc-create-imenu-index ()
  "Create an imenu index for the current buffer."
  (goto-char (point-min))
  (let (index start-match end-match)
    (while (setq start-match (text-property-search-forward
			      'face nil
			      #'perl-doc--heading-properties))
      (let* ((heading-properties (perl-doc--heading-properties
				  nil
				  (prop-match-value start-match)))
	     (face (car heading-properties))
	     (level (cdr heading-properties)))
	(setq end-match (text-property-search-forward
			 'face face
			 #'perl-doc--heading-face-end-p))
	(let* ((start (prop-match-beginning start-match))
	       (end (prop-match-beginning end-match))
	       (marker (copy-marker start))
	       (text (concat
		      (make-string (1- level) ?. )
		      (buffer-substring-no-properties start end))))
	  (push (cons text marker) index))))
      (reverse index)))

(define-derived-mode perl-doc-mode special-mode "perl-doc"
  "A mode for displaying Perl documentation.
The following key bindings are currently in effect in the buffer:
\\{perl-doc-mode-map}"
  :interactive nil
  (setq buffer-auto-save-file-name nil)
  (buffer-disable-undo)
  (auto-fill-mode -1)
  (add-hook 'window-size-change-functions #'perl-doc--auto-refresh nil t)
  (set-buffer-modified-p nil)
  (setq-local bidi-paragraph-direction 'left-to-right)
  ;; Creating the index only works with shr faces which have been
  ;; added in Emacs 28
  (when (perl-doc--index-available-p)
    (setq-local imenu-create-index-function #'perl-doc-create-imenu-index)))

(defun perl-doc-goto-section (section)
  "Find SECTION in the current buffer.
There is no precise indicator for SECTION in shr-generated
buffers, so this function is using some fuzzy regexp matching
which takes into account that the perldoc/pod2html workflow has
no clear specification what makes a section."
  (goto-char (point-min))
  ;; Here's a workaround for a misunderstanding between pod2html and
  ;; shr: pod2html converts a section like "/__SUB__" to a fragment
  ;; "#SUB__".	The shr renderer doesn't pick id elements in its
  ;; character properties, so we need to sloppily allow leading "__"
  ;; before looking for the text of the heading.
  (let ((target-re (replace-regexp-in-string "-" "." (regexp-quote section)))
	(prefix "^\\(__\\)?")
	(suffix "\\([[:blank:]]\\|$\\)"))
    (if (re-search-forward (concat prefix target-re suffix) nil t)
	(goto-char (line-beginning-position))
      (message "Warning: No section '%s' found." section))))

(defmacro perl-doc-with-L-grammar (&rest body)
  "Execute BODY with rx extensions for POD's L<...> element.
In Perl's documentation format POD, the link element L<...>
is the most complex.  This macro defines syntactic components
which allow to process these elements with some confidence."
  `(rx-let
       ((backslash ?\\)
	(double-quote ?\")
	(escaped (char) (sequence backslash char))
	(quoted (sequence double-quote
			  (zero-or-more
			   (or
			    (escaped backslash)
			    (escaped double-quote)
			    (not double-quote)))
			  double-quote))
	(plain (not (any "|<>")))	; no link nor markup special chars
	(extended (not (any "|/")))	; markup is ok, separators are not ok
	(unrestricted (seq (not ?/) (* any))) ; not starting with a slash
	(not-markup (seq (not (any "A-Z")) "<")) ; A "harmless" less-than char
	(markup-start  (sequence (in "A-Z") "<"))
	(link-start    (sequence "L<" (optional (group-n 1 (1+ "<") " "))))
	(simple-markup (sequence
			markup-start
			(+? (or
			     (not (any "<>/"))
			     not-markup))
			">"))
	(extended-markup (sequence
			  (in "A-Z") "<<" space ; opening phrase
			  ;; Delimiters are forbidden in links,
			  ;; allowed elsewhwere.  We can ignore
			  ;; this since we only treat links here)
			  (+? any)
			  space ">>")) ; ending phrase
	(markup			  ; We allow _one_ level of nesting
	 (or extended-markup
	     (sequence markup-start
		       (+? (or extended-markup
			       simple-markup
			       not-markup
			       (not (any "/>"))))
		       ">")))
	;; Now these are the things we're actually after: The parts
	;; that make a L<name|url> link.  We expect either an URL
	;; or a name for the target.
	(component (or plain markup not-markup))
	(name (group-n 2 (zero-or-more
			  (or (not (any " \"\t|/<>"))
			      markup))))
	(url (group-n 2 (sequence (one-or-more alpha) ; protocol
				  ":/"
				  (one-or-more (not (any " |<>"))))))
	;; old-style references to a section in the same page.
	;; This style is deprecated, but found in the wild.  We are
	;; following the recommended heuristic from perlpodspec:
	;;    .... if it contains any whitespace, it's a section.
	;; We also found quoted things to be sections.
	(old-section
	 (group-n 2
	   (or (sequence (1+ component) blank (1+ component))
	       quoted)))
	(text-simple (group-n 1 (+? component)))
	(section-simple (group-n 3 (or quoted (+ component))))
	(link-re-simple (sequence
			 point
			 (? (sequence text-simple "|" (? space)))
			 (or url
			     (sequence name (? (sequence "/" section-simple)))
			     old-section)
			 ">"))
	(text-extended (group-n 1 (+? extended)))
	(section-extended (group-n 3 (or quoted unrestricted)))
	(link-re-extended (sequence
			   point
			   (? (or text-extended (? space)))
			   (or url
			       (sequence name (? (sequence "/" section-extended)))
			       old-section)
			   ))
	)
     ,@body))

(defun perl-doc--process-links ()
  "Find the next link in a POD section, and process it.
The L<...> syntax is the most complex markup in the POD family of
strange things.	 Also, quite a lot of modules on CPAN and
elsewhere found ways to violate the spec in interesting ways
which seem to work, at least, with some formatters."
  ;; Note: Processing links can't be done with syntax tables by using
  ;; <> as a bracket pair because links can contain unbalanced < or >
  ;; symbols.  So do it the hard way....
  (goto-char (point-min))
  ;; Links, in general, have three components: L<text|name/section>.
  ;; "text" is what POD readers should display. "name" is the link target
  ;; (a POD file or a Perl module), and "section" is an anchor within
  ;; the link target.
  ;; In the following we match and capture like this:
  ;; - (match-string 1) to text, which is optional
  ;; - (match-string 2) to name, which is mandatory but may be empty
  ;;   for targets in the same file.   We capture old-style sections
  ;;   here, too, because syntactically they look like names.
  ;; - (match-string 3) to section.
  ;; Links can contain markup, too.  We support two levels of nesting
  ;; (because we've seen such things in the wild), but only with
  ;; single <> delimiters.  For the link element as a whole,
  ;; L<<< stuff >>> is supported.
  (perl-doc-with-L-grammar
   (while (re-search-forward (rx link-start) nil t)
     (let* ((terminator-length (length (match-string 1)))
	    (allow-angle (> terminator-length 0)); L<< ... >>
	    (re (if allow-angle (concat (rx link-re-extended)
					(make-string terminator-length ?>))
		  (rx link-re-simple)))
	    (end-marker (make-marker)))
       (re-search-forward re nil t)
       (set-marker end-marker (match-end 0))
       (cond
	((null (match-string 2))
	 ;; This means that the regexp failed.	Either the L<...>
	 ;; element is really, really bad, or the regexp isn't
	 ;; complicated enough.	 Since the consequences are rather
	 ;; harmless, don't raise an error.
	 (when perl-doc--debug
	   (message "perl-doc: Unexpected string: %s"
		    (buffer-substring (line-beginning-position)
				      (line-end-position)))))
	((string= (match-string 2) "")
	 ;; L<Some text|/anchor> or L</anchor> -> don't touch
	 nil)
	((save-match-data
	   (string-match "^\\w+:/" (match-string 2)))
	 ;; L<https://www.perl.org/> -> don't touch
	 nil)
	((save-match-data
	   (string-match " " (match-string 2)))
	 ;; L<SEE ALSO> -> L<SEE ALSO|/"SEE ALSO">, fix old style section
	 (goto-char (match-end 2))
	 (insert "\"")
	 (goto-char (match-beginning 2))
	 (insert (concat (match-string 2) "|/\"")))
	((save-match-data
	   (and (match-string 1) (string-match (rx quoted) (match-string 2))))
	 ;; L<unlink1|"unlink1"> -> L<unlink1|/"unlink1">, as seen in File::Temp
	 (goto-char (match-beginning 2))
	 (insert "/"))
	((save-match-data
	   (string-match (rx quoted) (match-string 2)))
	 ;; L<"safe_level"> -> L<safe_level|/"safe_level">, as seen in File::Temp
	 (goto-char (match-beginning 2))
	 (insert (concat (substring (match-string 2) 1 -1) "|/")))
	((match-string 3)
	 ;; L<Some text|page/sect> -> L<Some text|perldoc:///page/sect>
	 ;; L<page/section> -> L<page/section|perldoc:///page/section>
	 ;; In both cases:
	 ;; Work around a bug in pod2html as of 2020-07-27: It
	 ;; doesn't grok spaces in the "section" part, though they
	 ;; are perfectly valid.  Also, it retains quotes around
	 ;; sections which it removes for links to local sections.
	 (let ((section (match-string 3))
	       (text (if (match-string 1) ""
		       (concat (match-string 3)
			       " in "
			       (match-string 2) "|"))))
	   (save-match-data
	     (setq section (replace-regexp-in-string "\"" "" section))
	     (setq section (replace-regexp-in-string " " "-" section)))
	   (goto-char (match-beginning 3))
	   (delete-char (- (match-end 3) (match-beginning 3)))
	   (insert section)
	   (goto-char (match-beginning 2))
	   (insert text)
	   (insert "perldoc:///")))
	((match-string 1) ; but without section
	 ;; L<Some text|page> -> L<Some text|perldoc:///page>
	 (goto-char (match-beginning 2))
	 (insert "perldoc:///"))
	;; ((match-string 3)
	;;  ;; L<page/section> -> L<page/section|perldoc:///page/section>
	;;  ;; Work around a bug in pod2html as of 2020-07-27, see above
	;;  (goto-char (match-beginning 2))
	;;  (insert (concat (match-string 3) " in " (match-string 2)
	;;		  "|" "perldoc:///")))
	(t
	 ;; L<page> -> L<page|perldoc:///page>
	 (goto-char (match-beginning 2))
	 (insert (concat (match-string 2) "|" "perldoc:///"))))
       (goto-char (marker-position end-marker))))))

(defvar-local perl-doc-base nil)
(defvar-local perl-doc--current-topic nil)
(defvar-local perl-doc--current-type nil)
(defvar-local perl-doc--current-section nil)
(defvar-local perl-doc-text-scale nil)
(defvar-local perl-doc-window-width 0
  "The width of a perl-doc-mode window in pixels.  Used to avoid
unneccesary calls to -refresh: If only the height of the window
changes (which easily happens due to minibuffer and echo-area
activities), then we do not redraw.")
(defvar-local perl-doc-module-history nil
  "This list keeps track of the documentation read so far.")

(defun perl-doc--collect-perl5lib ()
  "Build a suitable PERL5LIB environment variable.
Uses the customizable value `perl-doc-extra-libs' and the
existing value of PERL5LIB.  Either one, or both, may be empty."
  (let ((extra-libs (if perl-doc-extra-libs
			(mapconcat #'expand-file-name
				   perl-doc-extra-libs
				   path-separator)))
	(perl5lib (getenv "PERL5LIB")))
    (if (and extra-libs perl5lib)
	(concat extra-libs path-separator perl5lib)
      (or extra-libs perl5lib))))

(defun perl-doc--fetch-pod (topic type)
  "Fetch the pod document for WORD.
WORD is the name of either a module, a file name, a function or a
variable.  We use heuristics to identify functions (from
cperl-mode shortdocs) and variables (from their first character
being a sigil)."
  ;; Add our extra-libraries to PERL5LIB
  (let ((args (pcase type
		('perl-variable (list "-v" topic))
		('perl-function (list "-f" topic))
		(_              (list      topic)))))
    (perl-doc--with-environment-variables
	(("PERL5LIB" (perl-doc--collect-perl5lib)))
      ;; Fetch plain POD into the current buffer
      (not (< 0 (apply #'call-process
		       (append
			(list perl-doc-perldoc-program nil t t "-u")
			args)))))))

(defun perl-doc--render-pod ()
  "Render the POD in the current buffer.
As a first step, process links in the POD document. Then,
converted POD to HTML with the external program pod2html, and
then render that HTML with `shr-render-region'."
  (shell-command-on-region (point-min) (point-max)
			   (concat perl-doc-pod2html-program
				   " --cachedir="
				   (make-temp-file "perl-doc" t)
				   " --flush"
				   " --noindex"
				   " --quiet")
			   (current-buffer) t "*perldoc error*")
  (shr-render-region (point-min) (point-max) (current-buffer))
  (deactivate-mark)
  ;; Remove shr's keymap for links which would shadow our mode's keymap
  (remove-text-properties (point-min) (point-max) '(keymap nil)))

(defun perl-doc--completion-check-choice (choice)
  "Check whether the CHOICE made during completion is valid.
The purpose is to prevent exiting with a trailing \"::\" which
does not lead to valid POD but needs another completion.  We do
permit, however, things which look like a variable or a function,
since there is no completion for those."
  (null (string-match "::\\'" choice)))

(defun perl-doc--common (topic type &optional section noselect)
  "Get Perl documentation for TOPIC of TYPE, the go to SECTION.
If NOSELECT, do not pop to the buffer.  This is the common
backend for `perl-doc', `perl-doc-function' and others.  TYPE is
one of the symbols m, f or v, declaring TOPIC as a module, a
function or a variable."
  (let* ((case-fold-search nil)
	 (buffer-name-prefix '((perl-function . "f/")
			       (perl-variable . "v/")))
	 (perldoc-buffer (concat "*perldoc-"
				(cdr (assoc type buffer-name-prefix))
				(substring-no-properties topic)
				"*")))
    (unless (get-buffer perldoc-buffer)
      ;; Jump to the relevant frame (if applicable)
      (dframe-select-attached-frame)
      (with-current-buffer (get-buffer-create perldoc-buffer)
	;; We do this early, so that our buffer-local variables can be
	;; set without being clobbered by a subsequent `perl-doc-mode'.
	(perl-doc-mode)
	(let ((inhibit-read-only t))
	  (when (perl-doc--fetch-pod topic type)
	    (perl-doc--process-links)
	    (perl-doc--render-pod)
	    (when perl-doc-text-scale
	      (setq-local text-scale-mode-amount perl-doc-text-scale)
	      (text-scale-mode nil))
	    (if section
		(perl-doc-goto-section section)
	      (goto-char (point-min)))))
	(setq-local revert-buffer-function #'perl-doc--refresh
		    perl-doc--current-topic topic
		    perl-doc--current-type type
		    perl-doc--current-section section)
	(setq-local perl-doc-window-width
		    (window-body-width (selected-window) t))))
  ;; Eventually, show the buffer and store current variables
  (or noselect
      (pop-to-buffer perldoc-buffer))))

;;;###autoload
(defun perl-doc (word &optional section)
  "Get Perl documentation like the perldoc command.
Does better formatting than man pages, including hyperlinks."
  ;; Process user input to retrieve WORD
  (interactive
   (let* ((default (cperl-word-at-point))
	  (read (completing-read
		 (perl-doc--format-prompt
		  "Find doc for Perl topic" default)
		 #'perl-doc--complete-module
		 nil
		 ;; Emacs 29 or newer needed for
		 ;; REQUIRE-MATCH to be a function
		 (if (>= emacs-major-version 29)
		     #'perl-doc--completion-check-choice
		   nil)
		 nil
		 'perl-doc-module-history
		 default)))
     (list (if (equal read "")
	       default
	     read))))
  ;; Apply heuristics to detect the type
  (let ((type
	 (cond
	  ((string-match (rx string-start (in "$@%")) word)
	   (setq-local perl-doc-base "perlvar")
	   'perl-variable)
	  ((and
	    (string-match "^\\(-[A-Za-z]\\|[a-z2]+\\)$" word)
	    (string-match (concat "^" word "\\>")
			  (documentation-property
			   'cperl-short-docs
			   'variable-documentation))
	    ;; FIXME: Ugly disambiguation between pragma and function
	    (not (string= word "open")))
	   (setq-local perl-doc-base "perlfunc")
	   'perl-function)
	  (t
	   (setq-local perl-doc-base nil)
	   'module))))
    ;; Eventually, do the hard work
    (perl-doc--common word type section)))

;;;###autoload
(defun perl-doc-function (topic)
  "Get Perl documentation for a builtin function WORD.
This retrieves the corresponding section from the perlfunc page.
No completion is done (yet).  Sorry."
  (interactive "MFunction name: ")
  (perl-doc--common topic 'perl-function))
    
;;;###autoload
(defun perl-doc-variable (topic)
  "Get Perl documentation for a builtin function WORD.
This retrieves the corresponding section from the perlfunc page.
No completion is done (yet).  Sorry."
  (interactive "MVariable name: ")
  (perl-doc--common topic 'perl-variable))
    
;;;###autoload
(defun perl-doc-file (file)
  "Run `perl-doc' on FILE.
This is the same as running `perl-doc' with FILE as an argument,
but provides file-name completion."
  (interactive "f")
  (let ((absolute-path (expand-file-name file)))
    (setq-local default-directory (file-name-directory absolute-path))
    (perl-doc--common absolute-path 'module)))

;; Make elint-current-buffer happy
(defvar text-scale-mode-amount)		; in face-remap.el, which we require

(defun perl-doc--buffer-namespace (buffer-name)
  "Return the Perl namespace for the buffer named BUFFER-NAME.
Return nil if the buffer name does not fit the perl-doc schema
or if it fits the perl-doc schema for a file name."
  (and (string-match (rx string-start "*perldoc-"
			 (group (+ (not (in "/"))))
			 "*"  string-end)
		     buffer-name)
       (match-string 1 buffer-name)))

(defun perl-doc--refresh (&optional _ignore-auto _noconfirm)
  "Refresh the current piece of documentation."
  (when (string-equal major-mode "perl-doc-mode")
    (let ((inhibit-read-only t)
	  (position (point))
	  (scale (if (and (boundp 'text-scale-mode) text-scale-mode)
		     text-scale-mode-amount
		   nil)))
      (erase-buffer)
      (when (perl-doc--fetch-pod perl-doc--current-topic
				 perl-doc--current-type)
	(perl-doc--process-links)
	(perl-doc--render-pod)
	(goto-char (min position (point-max)))
	(when scale
	  (setq-local text-scale-mode-amount scale)
	  (text-scale-mode nil))))))

(defvar perl-doc--window-size-change-timer nil)

(defun perl-doc--auto-refresh (window)
  "Reformat the page after a change of window size"
  (when (and (window-live-p window)
	     (/= (window-body-width window t)
		 perl-doc-window-width))
    (setq-local perl-doc-window-width
		(window-body-width window t))
    (when (timerp perl-doc--window-size-change-timer)
      (cancel-timer perl-doc--window-size-change-timer))
    (setq perl-doc--window-size-change-timer
	  (run-with-idle-timer 1 nil #'perl-doc--refresh))))

(defun perl-doc-browse-url ()
  "Browse the URL at point, using either perldoc or `shr-browse-url'.
If the URL at point starts with the \"perldoc\" schema, then run
either perl-doc, or produce a man-page if the URL is of the type
\"topic(section)\".  If it is a local fragment, just search for
it in the current buffer.  For URLs with a schema, run
browse-url."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (pcase url
      ((pred (string-match
	      (concat "^perldoc:///"	; our scheme
		      "\\(?:\\(?1:[^/]*\\)"   ; 1: page, may be empty
		      "\\(?:#\\|/\\)"	   ; section separator
		      "\\(?2:.+\\)" ; "/" + 2: nonzero section
		      "\\|"		; or
		      "\\(?1:.+\\)\\)$")))   ; 1: just a page
       ;; link to be handled by perl-doc
       (let ((page   (match-string 1 url))
	     (section (match-string 2 url)))
	 (if (> (length page) 0)
	     (if (string-match "([1-9])$" page)
		 (man page)
	       (perl-doc--common page 'module section))
	   (when section
	     (perl-doc-goto-section section)))))
      ((pred (string-match "^#\\(.+\\)"))
;;       ((string-match "^#\\(.+\\)" url)
	;; local section created by pod2html
       (if perl-doc-base
	   (perl-doc--common perl-doc-base 'module
			     (match-string-no-properties 1 url))
	 (perl-doc-goto-section (match-string-no-properties 1 url))))
      (_
       (shr-browse-url)))))

(defvar perl-doc-speedbar-nodes
  (make-hash-table :test #'equal :size 60)
  "Cache the nodes where documentation was found.")

(defun perl-doc-view-source ()
  "Visit the file which contains the POD source of the current buffer."
  (interactive)
  (let ((topic perl-doc--current-topic)
	(pod-source))
    (with-temp-buffer
      (if (= 0
	     (perl-doc--with-environment-variables
		 (("PERL5LIB" (perl-doc--collect-perl5lib)))
	       (call-process perl-doc-perldoc-program nil t t "-l" topic)))
	  (progn
	    (setq pod-source
		  (buffer-substring (point-min) (1- (point-max))))
	    (view-file pod-source))
	;; perldoc did not provide a source file.
	;; Let's try the speedbar-nodes!
	(string-match (rx string-start
			  (group (? (: (* any) "::")))
			  (group (+ (not (in ":"))))
			  string-end)
		      topic)
	(let* ((nodes (progn
			(gethash (match-string 1 topic)
				 perl-doc-speedbar-nodes nil)))
	       (node (assoc topic nodes))
	       (file-names (when node
			      (cddr node))))
	  (pcase (length file-names)
	    (0 nil)
	    (1 (view-file (car file-names)))
	    (_
	     (setq pod-source
		   (completing-read "Select POD source: "
				    file-names
				    nil nil nil nil
				    (car file-names)))
	    (view-file pod-source))))))))

;;; Completion for documentation topics

(defun perl-doc--complete-collect-candidates (input)
  "Collect the list of valid completions for INPUT.
Collects module names and Perl core documentation, but not
variables.  This function is the backend for
`perl-doc-complete-module' which can be used in
`completing-read'."
  (let ((namespace "")
	(candidate input)
	(depth 0)
	(nodes (perl-doc--collect-directories
		(perl-doc--root-directories) ""))
	ancestor
	ancestor-node)
    (while (string-match (rx string-start
			     (group (* (not (in ":")))
				    "::")
			     (group (* any)))
			 candidate)
      (setq ancestor (match-string 1 candidate)
	    candidate (match-string 2 candidate)
	    namespace (concat namespace ancestor)
	    ancestor-node (assoc namespace nodes)
	    nodes (perl-doc--collect-directories
		   (cddr ancestor-node) namespace)
	    depth (1+ depth)))
    (mapcar #'car nodes)))

(defun perl-doc--complete-module (input _predicate flag)
  "Complete INPUT to the next namespace separator.
  Collects the list of candidates as a function of INPUT and then
  calls the basic completion functions with this list."
   (pcase flag
     ('nil
      (try-completion input (perl-doc--complete-collect-candidates input)))
     ('t
      (all-completions input (perl-doc--complete-collect-candidates input)))
     ('lambda
       (test-completion input (perl-doc--complete-collect-candidates input)))
     (`('boundaries ,suffix)
    ;; The default from `cperl-word-at-point' can be a
    ;; Deeply::Nested::Module, so we want to verify the whole path.
    ;; Let's return the basic default, as mentioned in the
    ;; documentation.
      `(boundaries 0 . ,(length suffix)))
     ('metadata
      ;; Not used yet.
      nil)))

;;;
;;; Speedbar support
;;;

(declare-function speedbar-make-specialized-keymap "speedbar" ())
(declare-function speedbar-add-expansion-list "speedbar" (new-list))
(declare-function speedbar-add-mode-functions-list "speedbar" (list))
(defvar speedbar-last-selected-file nil
  "The last file which was selected in speedbar buffer.
We need to override that when switching to perl-doc display.")

(defvar perl-doc--browser-p nil
  "Indicates whether `perl-doc-browser' has been started.
If t, then `perl-doc-speedbar-buttons' will not switch to another
mode if the current buffer is not in perl-doc-mode.")

(defun perl-doc-browser ()
  "Use a `speedbar' frame to browse Perl documentation on your system.
This uses a speedbar major display mode."
  (interactive)
  (require 'speedbar)
  (setq perl-doc--browser-p t)
  (speedbar-change-initial-expansion-list "perl-doc")
  (speedbar-frame-mode 1)
  (speedbar-refresh)
  )

(defvar perl-doc-speedbar-keymap nil
  "A keymap for use in the perl-doc view of Speedbar.")

(defun perl-doc--speedbar-key-RET ()
  "Handle <RET> in the perl-doc view.
Calls whatever is in the text-property \"speedbar-function\" at
point."
  (interactive)
  (let ((func (get-text-property (point) 'speedbar-function)))
    (when func
      (funcall func (speedbar-line-text)
	       (speedbar-line-token)
	       (perl-doc--speedbar-current-depth)))))

(defun perl-doc--speedbar-key-+ ()
  "Expand the view for the current line, if possible.
If there's a \"+\" in the current line, call the appropriate
function.  Otherwise, do nothing."
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (when (search-forward-regexp "[<[{]\\+" nil t)
      (let ((func (get-text-property (point) 'speedbar-function)))
	(when func
	  (funcall func (speedbar-line-text)
		   (speedbar-line-token)
		   (perl-doc--speedbar-current-depth)))))))

(defun perl-doc-quit-browser ()
  "Stop displaying the speedbar in \"perl-doc\" display.
If the browser was active, then this does not close the speedbar
frame, but puts the selection of the speedbar display back to
automatic selection.  If the browser was not active, close the
speedbar frame."
  (interactive)
  (if perl-doc--browser-p
      (progn
	(setq perl-doc--browser-p nil)
	(speedbar-refresh))
    (dframe-close-frame)))

(defun perl-doc--speedbar-view-source ()
  "View the POD source of the current line in speedbar."
  (interactive)
  (let ((perl-doc--current-topic (car (speedbar-line-token))))
    (perl-doc-view-source)))

(defun perl-doc--install-speedbar-variables ()
  "Do whatever is needed to fire up speedbar for POD files"
  (or perl-doc-speedbar-keymap
      (let ((map (speedbar-make-specialized-keymap)))
	(define-key map "\C-m" #'perl-doc--speedbar-key-RET)
	(define-key map "+" #'perl-doc--speedbar-key-+)
	(define-key map "q" #'perl-doc-quit-browser)
	(define-key map "v" #'perl-doc--speedbar-view-source)
	(setq perl-doc-speedbar-keymap map))))

(with-eval-after-load 'speedbar
  (perl-doc--install-speedbar-variables))

(defun perl-doc-speedbar-line-directory (&optional _depth)
  "The function to serve as `speedbar-line-directory'"
  (pcase (speedbar-line-token)
    ('nil
     ;; No token: Return the current buffer name in the attached frame
     (buffer-name
      (window-buffer (frame-selected-window
		      (speedbar-select-attached-frame)))))
    ((pred listp)
     ;; "our" token: Return the current buffer name in the attached frame
     (buffer-name
      (window-buffer (frame-selected-window
		      (speedbar-select-attached-frame)))))
    ((pred markerp)
     ;; a tag, return the buffer's file name
     ;; FIXME: That's broken because docs have no file name
     (buffer-file-name (marker-buffer (speedbar-line-token))))
    (_
     (message "I don't know what to do with text '%s' and token '%s'"
	      (speedbar-line-text) (speedbar-line-token)))))

(defun perl-doc-speedbar-item-info ()
  "The function to serve as `speedbar-item-info' in perl-doc view."
  (pcase (speedbar-line-token)
    ((pred listp)
    ;; "our" token
     (let* ((node (speedbar-line-token))
  	    (namespace (car node))
  	    (type (nth 1 node)))
       (if (eq type 'directory)
	   (if (equal namespace "")
	       (speedbar-line-text)
  	     namespace)			; Directory
  	 (concat namespace " in " (nth 2 node)))))
    ((pred markerp)
     ;; An imenu tag (at least we hope so)
     (concat "Heading: " (speedbar-line-text)))))

(defvar perl-doc-speedbar-menu-items
  '(["Expand" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    ["Render POD" perl-doc-speedbar-view-pod
     (equal (cadr (speedbar-line-token)) 'file)]
    ["View POD source" perl-doc--speedbar-view-source
     (equal (cadr (speedbar-line-token)) 'file)]
    )
  "The menu for browsing Perl documentation.")

(defun perl-doc-activate-speedbar-display ()
  "Use perl-doc to view your speedbar entries."
  (interactive)
  (speedbar-add-expansion-list '("perl-doc"
                                 perl-doc-speedbar-menu-items
                                 perl-doc-speedbar-keymap
                                 perl-doc-speedbar-buttons))
  (speedbar-add-mode-functions-list
   '("perl-doc"
     (speedbar-item-info . perl-doc-speedbar-item-info)
     (speedbar-line-directory . perl-doc-speedbar-line-directory))))

(defun perl-doc--root-directories ()
  "Return the list of directories to be used used by `perl-doc'.
The list is built from the customizable list
`perl-doc-extra-libs' and the array @INC as used by Perl itself.
Directories which don't exist are eliminated, as well as any
non-directory entry in @INC."
  (let ((roots (append perl-doc-extra-libs
		       (with-temp-buffer
			 (call-process "perl"
				       nil t nil
				       "-E" "say for @INC")
			 (split-string (buffer-string) "\n" t)))))
    (cl-remove-if-not #'file-exists-p roots)))


;; This function is no longer used.  It turned out to be too slow when
;; used for filtering out files which don't have POD.
(defun perl-doc--pod-source-p (file)
  "Return true if FILE contains POD.
Delegates the examination to `perl-doc-perldoc-program' which
should yield a nonzero exit code if FILE does not contain usable
POD."
  (if (equal 0
	     (perl-doc--with-environment-variables
		 (("PERL5LIB" (perl-doc--collect-perl5lib)))
	       (call-process perl-doc-perldoc-program
			     nil nil nil "-l" file)))
      t					; exit code = 0, POD found
    nil))				; exit code != 0, no POD

(defun perl-doc--nodes-by-name (n1 n2)
  "Return t if node N1 is to be sorted before N2.
N1 and N2 are lists (namespace type file) where file is an
absolute path and type indicates either a file or a directory.
The namespace part has highest priority for sorting.  The
namespace of directories has a trailing \"::\", so it is
automatically sorted after the files."
  (let* ((name1 (car n1)) (type1 (nth 1 n1)) (file1 (nth 2 n1))
	 (name2 (car n2)) (type2 (nth 1 n2)) (file2 (nth 2 n2)))
    (cond
     ;; The easy part: Different basenames
     ((string-lessp name1 name2) t)
     ((string-lessp name2 name1) nil)
     ;; Different types: Files before directories (should not be used ever)
     ((and (eq type1 'file) (eq type2 'directory)) t)
     ((and (eq type2 'file) (eq type1 'directory)) nil)
     ;; Different extensions: "pod" to be preferred
     ((string= (file-name-extension file1) "pod") t)
     ((string= (file-name-extension file2) "pod") nil)
     (t 				; Fallback: Just compare
      (string-lessp file1 file2)))))

(defvar directory-files-no-dot-files-regexp nil
  "Defined in files.el.")

(defun perl-doc--collect-directory (directory namespace)
  "Collect files and directories from DIRECTORY under NAMESPACE.
NAMESPACE is the Perl namespace for DIRECTORY which will be
prepended to the namespace entries in the results.  Files with
extension \"pod\" and directories are always included.
Directories with the names \"pod\" and \"pods\" are immediately
expanded.  Files with extensions \"pl\", \"PL\", \"perl\" and
\"pm\" are included if they contain some POD, unless there is a
file with the same basename ending in \"pod\".

Returns an unsorted list of 3-element lists (namespace type
filename)."
  (let ((contents (directory-files directory t
				   directory-files-no-dot-files-regexp))
	results)			; we return this value
    (dolist (element contents)
      (if (file-directory-p element)
	  ;; Directories
	  (if (member (file-name-nondirectory element) '("pod" "pods"))
	      ;; Directories named "pod" or "pods" are scanned by
	      ;; perldoc, but that name is not part of the
	      ;; namespace.  Therefore we expand them immediately.
	      (let ((pods (perl-doc--collect-directory
			   element namespace)))
		(dolist (pod pods)
		  (push pod results)))
	    ;; Every other directory name is part of the namespace
	    (push (list (concat (or namespace "")
				(file-name-base element)
				"::")
			'directory
			element)
		  results))
	;; Files
	(when (null (backup-file-name-p element)) ; ignore backup files
	  (let ((extension (file-name-extension element))
		(ignored-files '("open2.pl" "open3.pl")))
	    (if extension
		(when
		    (or (equal extension "pod")
			(and (member extension '("pl" "PL" "perl" "pm"))
			     ;; Certain old stuff has no docs anyway
			     (not (member (file-name-nondirectory element)
					  ignored-files))
			     ;; Prefer .pod over other extensions
			     (null (file-exists-p
				    ;; Only in Emacs 28 and newer:
				    ;; (file-name-with-extension ".pod")))))
				    (concat
				     (file-name-sans-extension element)
				     element "pod")))))
		  (push (list (concat (or namespace "")
				      (if (member extension '("pm" "pod"))
					  (file-name-base element)
					(file-name-nondirectory element)))
			      'file
			      element)
			results)))))))
    results))

(defun perl-doc--collect-directories (directories namespace)
  "Collect files and subdirectories for NAMESPACE from DIRECTORIES.
Each entry in the list DIRECTORIES is supposed to represent the
same Perl NAMESPACE, but starting from different roots in @INC.
This function assumes that each element of DIRECTORIES is an
existing directory.

Returns a list of nodes, each of which is a list where the first
element is the namespace, which is the key used for sorting.  The
second element is the type (\\='directory or \\='file).
Following elements are full filenames.  Every directory and
module can be present in more than one branch of the root
directories."
  (let ((nodes))
    (if (setq nodes (gethash namespace perl-doc-speedbar-nodes nil))
	;; namespace has been cached, return that entry
	nodes
      ;; NAMESPACE is not yet in cache.  Collect the entries and write
      ;; them to the hash, also return them.
      (dolist (directory directories)
	(let ((content (perl-doc--collect-directory directory namespace)))
	  (dolist (item content)
	    (let* ((namespace (car item))
		   (type (nth 1 item))
		   (file-name (nth 2 item)) ;
		   (found (assoc namespace nodes #'equal)))
	      (if found
		  ;; we already have an entry: Append the new path
		  (setcdr found (cdr (append found (list file-name))))
		;; This is a new entry for NAMESPACE
		(push (list namespace type file-name) nodes))))))
      ;; And return the collected stuff, sorted appropriately
      (setq nodes (sort nodes #'perl-doc--nodes-by-name))
      (puthash namespace nodes perl-doc-speedbar-nodes)
      nodes)))

(defun perl-doc-refresh-libs ()
  "Re-evaluate the file system for Perl modules and documentation.
This works by dropping the cache, so re-evaluation is again done on demand only."
  (interactive)
  (clrhash perl-doc-speedbar-nodes))

(defun perl-doc-add-lib (directory)
  "Add DIRECTORY to be scanned for Perl docs for this session.
DIRECTORY is inserted before the current list, similar to Perl's
\"use lib ...\".  The shortcut \"~\" for the home directory can
be used.  To add a directory permanently, either put this command
in your init file or use `customize-variable' for
`perl-doc-extra-libs'."
  (interactive "D")
  (add-to-list 'perl-doc-extra-libs directory))

(defun perl-doc--button-label (node)
  "Construct a label for a button for NODE.
Eliminates all \"Foo::\" words except the last one from the
node's namespace."
  (string-match "[^:]*\\(::\\)?\\'" (car node))
  (match-string 0 (car node)))

(defun perl-doc--insert-directory-button (node depth &optional label)
  "Insert a button for one directory at the current point."
   (let ((tag-button (or label (perl-doc--button-label node))))
    (speedbar-make-tag-line
     'angle ?+			 ; EXP-BUTTON-TYPE, -CHAR
     'perl-doc-speedbar-expand	 ; EXP-BUTTON-FUNCTION
     node			 ; EXP-BUTTON-DATA
     tag-button			 ; TAG-BUTTON
     'perl-doc-speedbar-toggle	 ; TAG-BUTTON-FUNCTION
     node			 ; TAG-BUTTON-DATA
     'speedbar-directory-face	 ; TAG-BUTTON-FACE
     depth			 ; DEPTH
     )))

(defun perl-doc--insert-file-button (node depth)
  "Insert a button for NODE at the current point, if defined."
  (when node
    (let ((tag-button (perl-doc--button-label node))
	  (button-char (if (perl-doc--index-available-p) ?+ ?\ ))
	  (button-function
	   (if (perl-doc--index-available-p)
	       'perl-doc-speedbar-insert-imenu
	     nil)))
      (speedbar-make-tag-line
       'bracket button-char		; EXP-BUTTON-TYPE, CHAR
       button-function                	; EXP-BUTTON-FUNCTION
       node				; EXP-BUTTON-DATA
       tag-button			; TAG-BUTTON
       'perl-doc-speedbar-view-pod	; TAG-BUTTON-FUNCTION
       node				; TAG-BUTTON-DATA
       'speedbar-file-face		; TAG-BUTTON-FACE
       depth				; DEPTH
       ))))

(defvar perl-doc--speedbar-headline
  "Perl Documentation:\n"
  "The line to start our display with.
Also used to detect whether the speedbar currently holds Perl
documentation.")

(defun perl-doc--speedbar-goto-insertion-point (name depth)
  "Go to the point where the button for NAME at level DEPTH should be.
This assumes that the point is positioned at the end of the line of
the entry of NAME's ancestor.  Returns t if the entry for NAME is
already there, nil otherwise."
  (let ((looking-for-insertion-point-p t)
	(entry-found-p nil))
    (while looking-for-insertion-point-p
      (setq looking-for-insertion-point-p
	    (and (setq entry-found-p
		       (search-forward-regexp
			(rx (group (+ (in "0-9"))) ":" (* " ")
			    (or (seq (in "[<{") any (in "]>}"))
				">")
			    " " (group (+ (not (in "\n")))))
		  (line-end-position 2) t))
		 (or (> (string-to-number (match-string 1)) depth)
		     (and (= (string-to-number (match-string 1)) depth)
			  (string< (match-string 2) name))))))
    ;; At the bottom there's no more following speedbar line
    (if entry-found-p (beginning-of-line) (forward-line)))
  (looking-at (concat "\\([0-9]+\\): +[<[{].[]>}] " name)))

(defun perl-doc--speedbar-insert-topic (topic)
  "Insert TOPIC in the current speedbar display."
  ;; Find the category
  (let ((namespace "")
	(nodes (perl-doc--collect-directories
		(perl-doc--root-directories) ""))
	(depth 0)
	ancestor ancestor-node)
    (goto-char (point-min))
    (search-forward-regexp
     (pcase topic
       ((pred perl-doc--language-documentation-p)
	"0:<.> Perl Language")
       ((pred perl-doc--module-documentation-p) 
	"0:<.> Modules")
       ((pred perl-doc--old-delta-p)
	"0:<.> Old perldeltas")))
    ;; process "Foo::" prefixes
    (while (string-match (rx string-start
			     (group (* (not (in ":")))
				    "::")
			     (group (+ any)))
			 topic)
      (setq ancestor (match-string 1 topic)
	    topic (match-string 2 topic)
	    namespace (concat namespace ancestor)
	    ancestor-node (assoc namespace nodes)
	    nodes (perl-doc--collect-directories
		   (cddr ancestor-node) namespace)
	    depth (1+ depth))
      (if (perl-doc--speedbar-goto-insertion-point ancestor depth)
	  (forward-line)
	(perl-doc--insert-directory-button ancestor-node depth)))
    ;; Now topic is a leaf corresponding to a file
    (setq namespace (concat namespace topic)
	  ancestor-node (assoc namespace nodes)
	  depth (1+ depth))
    (if (perl-doc--speedbar-goto-insertion-point topic depth)
	(forward-line)
      (perl-doc--insert-file-button (assoc namespace nodes) depth))))

(defun perl-doc-speedbar-buttons (directory &optional _depth)
  "Display the documentation tree of installed Perl modules in speedbar."
  (cond
   ;; Have we been called by speedbar-refresh for a perl-doc-mode buffer?
   ((and (get-buffer directory)
	 (with-current-buffer (get-buffer directory)
	   (eq major-mode 'perl-doc-mode))
	 (not (equal speedbar-initial-expansion-list-name "perl-doc")))
    ;; That triggers a refresh under "perl-doc" display
    (setq-local speedbar-last-selected-file nil)
    (speedbar-change-initial-expansion-list "perl-doc"))
   ;; Any non-perldoc buffer should not use perl-doc display
   ;; unless we are in `perl-doc-browser' mode
   ((and (get-buffer directory)
	 (not (eq (with-current-buffer (get-buffer directory)
		    major-mode)
		  'perl-doc-mode))
	 (equal speedbar-initial-expansion-list-name "perl-doc")
	 (not perl-doc--browser-p))
    (speedbar-change-initial-expansion-list
     (if (or (equal speedbar-previously-used-expansion-list-name "perl-doc")
	       (not speedbar-previously-used-expansion-list-name))
	 "files"
       speedbar-previously-used-expansion-list-name))
    (speedbar-center-buffer-smartly))
   ;; The perl-doc browser switches on perl-doc mode
   ((and perl-doc--browser-p
   	 (not (equal speedbar-initial-expansion-list-name "perl-doc")))
    (speedbar-change-initial-expansion-list "perl-doc"))
   ((and (not (get-buffer directory))
	 (not perl-doc--browser-p))
    (speedbar-change-initial-expansion-list
     (if (or (equal speedbar-previously-used-expansion-list-name "perl-doc")
	     (not speedbar-previously-used-expansion-list-name))
	 "files"
       speedbar-previously-used-expansion-list-name)))
   (t
    ;; Build the top level button hierarchy if it isn't already there
    (unless (save-excursion (goto-char (point-min))
			    (looking-at (regexp-quote
					 perl-doc--speedbar-headline)))
      (erase-buffer)
      ;; Insert the root entries
      (insert perl-doc--speedbar-headline)
      (perl-doc--insert-directory-button
       (append (list "" 'directory) (perl-doc--root-directories)) 0
       "Perl Language")
      (perl-doc--insert-directory-button
       (append (list "" 'directory) (perl-doc--root-directories)) 0
       "Modules")
      (perl-doc--insert-directory-button
       (append (list "" 'directory) (perl-doc--root-directories)) 0
       "Old perldeltas"))
    ;; Now insert all perl-doc buffers in the list
    (dolist (buff (buffer-list))
      (let ((topic (perl-doc--buffer-namespace (buffer-name buff))))
	(and topic
	     (perl-doc--speedbar-insert-topic topic)))))))

(defun perl-doc-speedbar-expand (name node &optional depth)
  "Expand one level of buttons below NODE."
  (speedbar-with-writable
    (if (perl-doc--string-search "-" name)
	(perl-doc--speedbar-contract depth)
      (perl-doc--speedbar-inflate node depth))))

(defun perl-doc-speedbar-toggle (_name node &optional depth)
  "Expand or contract one level of buttons below NODE"
  (speedbar-with-writable
    (if (perl-doc--string-search "-" (buffer-substring
			    (line-beginning-position) (point)))
	(perl-doc--speedbar-contract depth)
      (perl-doc--speedbar-inflate node depth))))

(defun perl-doc--speedbar-contract (depth)
  "Contract the expansion below point up to DEPTH."
  (speedbar-change-expand-button-char ?+)
  (speedbar-delete-subblock depth))

(defun perl-doc--speedbar-current-namespace ()
  "Return the namespace of the current entry in speedbar.
Directory and module entries have the namespace in their token,
tags contan it in their marker."
  (let ((token (speedbar-line-token)))
    (pcase token
      ((pred listp)
       (car token))
      ((pred markerp)
       (let ((buffer-name (buffer-name (marker-buffer token))))
	 (perl-doc--buffer-namespace buffer-name))))))

(defun perl-doc--speedbar-current-depth ()
  "Return the numerical depth of the speedbar entry in the current line."
  (save-excursion
    (goto-char (line-beginning-position))
    (if (search-forward-regexp "[0-9]+" nil t)
	(string-to-number (match-string-no-properties 0))
    ;; At the end of the document: perldelta goes here
    1)))


;; This is a bit of a murky area... Curating and editing the
;; presentation of documentation beyond the "namespace" driven
;; information.  I (haj) just wanted to get the amount of the old
;; perldelta documents out of the way.  I don't like this
;; implementation, but failed to come up with a better plan.
(defun perl-doc--language-documentation-p (name)
  "Return t if NAME is part of Perl's builtin language documentation."
  (equal 0 (string-match (rx "perl" (or (in "a-z") "5db.pl" string-end)) name)))

(defun perl-doc--old-delta-p (name)
  "Return t if NAME is a perldelta documentation for an old version"
  (equal 0 (string-match (rx "perl5" digit) name)))

(defun perl-doc--module-documentation-p (name)
  "Return t if NAME is documentation for a Perl module or pragma."
  (not (or (perl-doc--language-documentation-p name)
	   (perl-doc--old-delta-p name))))

(defvar perl-doc--speedbar-inflate-filters
  '(("Modules" . perl-doc--module-documentation-p)
    ("Perl Language" . perl-doc--language-documentation-p)
    ("Old perldeltas" . perl-doc--old-delta-p))
  "Filters for special entries in the hierarchy")

(defun perl-doc--speedbar-inflate (node depth)
  "Expand one level of buttons below NODE, applying filters."
  (speedbar-change-expand-button-char ?-)
  (let* ((namespace (car node))
	 (directories (cddr node))
	 (nodes (perl-doc--collect-directories
		 directories namespace))
	 (depth (1+ depth))
	 (filter (cdr (assoc (speedbar-line-text)
			     perl-doc--speedbar-inflate-filters
			     #'equal))))
    (end-of-line)
    (forward-char)
    (dolist (node nodes)
      (let* ((node-name (car node))
	     (type (nth 1 node))
	     (current-depth (perl-doc--speedbar-current-depth))
	     (current-namespace (perl-doc--speedbar-current-namespace)))
	(while (> current-depth depth)
	  (forward-line)
	  (setq current-depth (perl-doc--speedbar-current-depth)))
	(if (and (= current-depth depth)
		 (string-equal node-name current-namespace))
	    (progn
	      (forward-line)
	      (setq current-depth (perl-doc--speedbar-current-depth)
		    current-namespace (perl-doc--speedbar-current-namespace)))
	  (when (or (not filter)
		    (funcall filter node-name))
	    (if (eq type 'directory)
		(perl-doc--insert-directory-button node depth)
	      ;; Files look different
	      (perl-doc--insert-file-button node depth))))))))

(defun perl-doc-goto-tag (_text tag-marker _level)
  "For the tag _TEXT, go to TAG-MARKER.
The marker contains information about the buffer."
  (let* ((doc-buffer (marker-buffer tag-marker)))
    (speedbar-select-attached-frame)
    (switch-to-buffer doc-buffer)
    (deactivate-mark)
    (goto-char (marker-position tag-marker))))

(defun perl-doc-tag-expand (name tag-markers depth)
  "For the button TEXT, expand the markers"
  (if (perl-doc--string-search "-" name)
      (perl-doc--speedbar-contract depth)
    (speedbar-change-expand-button-char ?-)
    (speedbar-with-writable
      (save-excursion
	(end-of-line)
	(forward-char)
	(speedbar-insert-generic-list depth tag-markers
				      #'perl-doc-tag-expand
				      #'perl-doc-goto-tag)))))

(defun perl-doc-speedbar-fetch-imenu (name)
  "Fetch imenu entries for NAME.
Return t if NAME is not a known Perl module."
  (speedbar-select-attached-frame)
  (perl-doc--common name 'module)
  (with-current-buffer (concat "*perldoc-" name "*")
    (funcall imenu-create-index-function)))

(defun perl-doc--speedbar-insert-imenu-list (depth toc)
  "Insert the table of contents TOC created by `imenu'.
This is very similar to `speedbar-insert-imenu-list' but uses our
own function to visit a tag: Speedbar assumes there's a file to
visit, but here we have a scratch buffer."
  (speedbar-insert-generic-list depth toc
				#'perl-doc-tag-expand
				#'perl-doc-goto-tag))

(defun perl-doc-speedbar-insert-imenu (button node depth)
  "Insert imenu entries for the file in NODE.
Return t if the name in NODE is not a known Perl module."
  (if (perl-doc--string-search "-" button)
      (perl-doc--speedbar-contract depth)
    (let* ((name (car node))
	   (toc (save-excursion (perl-doc-speedbar-fetch-imenu name))))
      (when toc
	(speedbar-change-expand-button-char ?-)
	(speedbar-with-writable
	  (save-excursion
	    (end-of-line)
	    (forward-char)
	    (perl-doc--speedbar-insert-imenu-list depth toc)))))))

(defvar imenu-create-index-function nil
  "Defined in imenu.el.")


;; (add-to-list 'speedbar-dynamic-tags-function-list
;; 	     '(perl-doc-speedbar-fetch-imenu . speedbar-insert-imenu-list))

(defun perl-doc-speedbar-view-pod (button node &optional _depth)
  "View the Perl documentation for the NODE given."
  (let ((name (car node)))
    (perl-doc--common name 'module)
    (when (string-match "\\+" button)
      (speedbar-expand-line))))

;; Eventually, activate our speedbar support
(defvar perl-doc--speedbar-initialized nil
  "Indicate whether our speedbar functions have been initialized.
This allows the buffer to be evaluated repeatedly.")

(when (not perl-doc--speedbar-initialized)
  (perl-doc-activate-speedbar-display)
  (setq perl-doc--speedbar-initialized t))

(provide 'perl-doc)
;;; perl-doc.el ends here

;;; perl-doc.el --- Read Perl documentation -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg <haj@posteo.de>
;; Created: 2022
;; Version: 0.2

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
;; human consumption.  The documentation used to be available in Emacs
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

;; We use some features from cperl-mode:
;;  * cperl-word-at-point  : Finding Perl syntax elements
;;  * cperl-short-docs	   : Tell functions from modules (for use with -f)

(require 'cperl-mode)
(require 'shr)
(require 'face-remap)

(defcustom perl-doc-pod2html-program "pod2html"
  "Path to the shell command pod2html."
  :type 'file
  :group 'perl-doc)

(defcustom perl-doc-perldoc-program "perldoc"
  "Path to the shell command perldoc."
  :type 'file
  :group 'perl-doc)

(defvar perl-doc--debug nil
  "If non-nil, unrecognized POD links are reported to the message buffer.
This is only relevant for developers, not for users.")

;; Make elint-current-buffer happy
(defvar button-buffer-map)		; in button.el
(defvar special-mode-map)		; in simple.el

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
  (when (facep 'shr-h1)
    (setq-local imenu-prev-index-position-function
		#'perl-doc--prev-index-position)
    (setq-local imenu-extract-index-name-function
		#'perl-doc--extract-index-name)))

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
	(not-delimiter (or (escaped "|") (escaped "/") (not (any "|/"))))
	(markup-start  (sequence (in "A-Z") "<"))
	(link-start    (sequence "L<" (optional (group-n 1 (1+ "<") " "))))
	(simple-markup (sequence
			markup-start
			(+? (or
			     (not (any "<>|/"))
			     not-markup))
			">"))
	(extended-markup (sequence
			  (in "A-Z") "<<" space ; opening phrase
			  ;; Delimiters are forbidden in links,
			  ;; allowed elsewhwere.  We can ignore
			  ;; this since we only treat links here)
			  (+? not-delimiter)
			  space ">>")) ; ending phrase
	(markup			  ; We allow _one_ level of nesting
	 (or extended-markup
	     (sequence markup-start
		       (+? (or extended-markup
			       simple-markup
			       not-markup
			       (not (any "|/>"))))
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
(defvar-local perl-doc-current-word nil)
(defvar-local perl-doc-current-section nil)
(defvar-local perl-doc-text-scale nil)

;;;###autoload
(defun perl-doc (word &optional section)
  "Get Perl documentation like the perldoc command.
Does better formatting than man pages, including hyperlinks."
  (interactive
   (let* ((default (cperl-word-at-point))
	  (read (read-string
		 (perl-doc--format-prompt "Find doc for Perl topic" default))))
     (list (if (equal read "")
	       default
	     read))))
  (let* ((case-fold-search nil)
	 (is-variable (string-match (rx line-start (in "$@%")) word))
	 (is-func (and
		   (string-match "^\\(-[A-Za-z]\\|[a-z]+\\)$" word)
		   (string-match (concat "^" word "\\>")
				 (documentation-property
				  'cperl-short-docs
				  'variable-documentation))))
	 (perldoc-buffer (concat "*perldoc-"
				 (substring-no-properties word)
				 "*")))
    (if (get-buffer perldoc-buffer)
	(pop-to-buffer perldoc-buffer)
      (with-temp-buffer
	;; for diagnostics comment out the previous line, and
	;; uncomment the next.	This makes the intermediate buffer
	;; permanent for inspection in the pod- and html-phase.
	;; (with-current-buffer (get-buffer-create (concat "**pod-" word "**"))
	;; Fetch plain POD into a temporary buffer
	(when (< 0 (cond
		    (is-variable
		     (call-process perl-doc-perldoc-program nil t t "-u" "-v" word))
		    (is-func
		     (call-process perl-doc-perldoc-program nil t t "-u" "-f" word))
		    (t
		     (call-process perl-doc-perldoc-program nil t t "-u" word))))
	  (error "%s" (buffer-string)))
	(perl-doc--process-links)
	(shell-command-on-region (point-min) (point-max)
				 (concat perl-doc-pod2html-program
					 " --cachedir="
					 (make-temp-file "perl-doc" t)
					 " --flush"
					 " --noindex"
					 " --quiet")
				 (current-buffer) t "*perldoc error*")
	(shr-render-buffer (current-buffer))) ; this pops to buffer "*html*"
      (switch-to-buffer "*html*") ; just to be sure
      (rename-buffer perldoc-buffer t)
      ;; Remove shr's keymap for links which would shadow our mode's keymap
      (remove-text-properties (point-min) (point-max) '(keymap nil))
      ;; FIXME: This kills all buffer-local variables
      (perl-doc-mode)
      (when perl-doc-text-scale
	(setq-local text-scale-mode-amount perl-doc-text-scale)
	(text-scale-mode nil)
	))
    (when section
      (perl-doc-goto-section section))
    (setq-local revert-buffer-function #'perl-doc--refresh
		perl-doc-base (if is-func "perlfunc"
				(if is-variable "perlvar" nil))
		perl-doc-current-word word
		perl-doc-current-section section)))

;;;###autoload
(defun perl-doc-file (file)
  "Run `perl-doc' on FILE.
This is the same as running `perl-doc' with FILE as an argument,
but provides file-name completion."
  (interactive "f")
  (perl-doc file)
  )

  ;; Make elint-current-buffer happy
(defvar text-scale-mode-amount)		; in face-remap.el, which we require

(defun perl-doc--refresh (&optional _ignore-auto _noconfirm)
  "Refresh the current piece of documentation."
  (when (string-equal major-mode "perl-doc-mode")
    (rename-buffer "*html*" t) ; ... so that shr re-uses this buffer
    (let ((inhibit-read-only t)
	  (position (point))
	  (scale (if (and (boundp 'text-scale-mode) text-scale-mode)
		     text-scale-mode-amount
		   nil)))
      (perl-doc perl-doc-current-word perl-doc-current-section)
      (goto-char position)
      (when scale
	(setq-local text-scale-mode-amount scale)
	(text-scale-mode nil)))))

(defvar perl-doc--window-size-change-timer nil)

(defun perl-doc--auto-refresh (window)
  "Reformat the page after a change of the window size"
  (when (window-live-p window)
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
    (when url
      (cond
       ((string-match (concat "^perldoc:///"	; our scheme
			      "\\(?:\\(?1:[^/]*\\)"   ; 1: page, may be empty
			      "\\(?:#\\|/\\)"	   ; section separator
			      "\\(?2:.+\\)" ; "/" + 2: nonzero section
			      "\\|"		; or
			      "\\(?1:.+\\)\\)$")   ; 1: just a page
		      url)
	;; link to be handled by perl-doc
	(let ((page   (match-string 1 url))
	      (section (match-string 2 url)))
	  (if (> (length page) 0)
	      (if (string-match "([1-9])$" page)
		  (man page)
		(perl-doc page section))
	    (when section
	      (perl-doc-goto-section section)))))
       ((string-match "^#\\(.+\\)" url)
	;; local section created by pod2html
	(if perl-doc-base
	    (perl-doc perl-doc-base
		      (match-string-no-properties 1 url))
	  (perl-doc-goto-section (match-string-no-properties 1 url))))
       (t
	(shr-browse-url))))))

(defun perl-doc-view-source ()
  "Visit the file which contains the POD source of the current buffer."
  (interactive)
  (let ((word perl-doc-current-word)
	(pod-source))
    (with-temp-buffer
      (call-process perl-doc-perldoc-program nil t t "-l" word)
      (setq pod-source (buffer-substring (point-min) (1- (point-max))))
      (view-file pod-source)
      )
    ))

;;; perl-doc-mode Index functions

(defvar perl-doc--heading-face nil
  "FIXME: Das werden wir noch los"
  )

(defun perl-doc--find-heading ()
  "Find the next heading"
  (interactive)
  (let (heading-start-match ; match object where the heading starts
	heading-end-match   ; match object after the heading
	from		    ; Start position of the heading
	to)		    ; End position of the heading
    (setq heading-start-match
	  (text-property-search-forward 'face
					t ; Any heading will do
					#'perl-doc--heading-face-p))
    (when heading-start-match
      (setq from (prop-match-beginning heading-start-match))
      (setq heading-end-match
	    (text-property-search-forward 'face
					  perl-doc--heading-face
					  #'perl-doc--heading-face-end-p))
      (when heading-end-match
	(setq to (prop-match-beginning heading-end-match))
	(buffer-substring-no-properties from to))
      )))

(defun perl-doc--prev-index-position ()
  "Find the previous index position.
To be used as `imenu-prev-index-position-function'."
  (let (heading-start-match ; match object where the heading starts
	heading-end-match)  ; match object after the heading
    (setq heading-end-match
	  (text-property-search-backward 'face
					 t ; Any heading will do
					 #'perl-doc--heading-face-p))
    (when heading-end-match
      (setq heading-start-match
	    (text-property-search-backward 'face
					   perl-doc--heading-face
					   #'perl-doc--heading-face-end-p))
      (when heading-start-match
	(goto-char (prop-match-end heading-start-match))
	(skip-syntax-forward "-")	; sometimes from points to NL
	(point))
      )))

(defun perl-doc--extract-index-name ()
  "Find the index name starting at point.
To be used as `imenu-extract-index-name-function'."
  (save-excursion
    (perl-doc--find-heading)))

(defun perl-doc--faces-heading-level (faces)
  "Check whether a list of FACES contains a heading and return its level.
Return nil if the list contains no face marking a heading."
  (let (level)
    (dolist (face faces level)
      (let ((name (face-name face)))
	(when (string-match (rx "shr-h" (group digit)) name)
	  (setq perl-doc--heading-face face)
	  (setq level (match-string-no-properties 1 name)))))))

(defun perl-doc--heading-face-p (_ got)
  "Check whether we GOT a heading value in the face we found.
We don't care which heading, therefore the expected value (first
 in the parameter list) is ignored."
  (let ((level (perl-doc--faces-heading-level
		(if (listp got) got (list got)))))
    (and level (string-match (rx digit) level))
    ))

(defun perl-doc--heading-face-end-p (expected got)
  "Find the first character where the face EXPECTED is not in GOT."
  (not (member expected (if (listp got) got (list got)))))

(provide 'perl-doc)
;;; perl-doc.el ends here

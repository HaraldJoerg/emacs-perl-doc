;;; perl-doc.el --- Read Perl documentation -*- lexical-binding: t -*-

;; Copyright (C) 2022 Harald Jörg

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg <haj@posteo.de>
;; Created: 2022
;; Version: 0.1

;; Keywords: languages
;; URL: https://github.com/HaraldJoerg/perl-doc

;; Package-Requires: ((emacs "28"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
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
;;
;;  * Uses the buffer's full width and can also be used with
;;    narrow buffers
;;
;; TODO list
;;
;;  * Documentation on Perl variables is yet to be implemented.
;;
;;  * The regex mechanism in `perl-doc--process-links` is a hack.  The
;;    author wrote this before he learned about rx and always meant to
;;    rewrite it in rx notation, but well, tuits.

;;; Code:

;; We use some features from cperl-mode:
;;  * cperl--format-prompt : A compatibility function for Emacs < 28
;;  * cperl-word-at-point  : Finding Perl syntax elements
;;  * cperl-short-docs     : Tell functions from modules (for use with -f)

(require 'cperl-mode)
(require 'shr)

(defcustom perl-doc-pod2html-program "pod2html"
  "Path to the shell command pod2html."
  :type 'file
  :group 'perl-doc
  :version 28)

(defcustom perl-doc-perldoc-program "perldoc"
  "Path to the shell command perldoc."
  :type 'file
  :group 'perl-doc
  :version 28)

(defvar perl-doc-shr-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'shr-next-link)
    (define-key map [?\M-\t] 'shr-previous-link)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'perl-doc-browse-url)
    (define-key map "\r" 'perl-doc-browse-url)
    (define-key map "q" 'bury-buffer)
    (define-key map (kbd "SPC") 'scroll-up-command)
    map)
  "A keymap to allow following links in perldoc buffers.")

(defun perl-doc-goto-section (section)
  "Find SECTION in the current buffer.
There is no precise indicator for SECTION in shr-generated
buffers, so this function is using some fuzzy regexp matching
which takes into account that the perldoc/pod2html workflow has
no clear specification what makes a section."
  (goto-char (point-min))
  ;; Here's a workaround for a misunderstanding between pod2html and
  ;; shr: pod2html converts a section like "/__SUB__" to a fragment
  ;; "#SUB__".  The shr renderer doesn't pick id elements in its
  ;; character properties, so we need to sloppily allow leading "__"
  ;; before looking for the text of the heading.
  (let ((target-re (replace-regexp-in-string "-" "." section))
	(prefix "^\\(__\\)?")
	(suffix "\\([[:blank:]]\\|$\\)"))
    (if (re-search-forward (concat prefix target-re suffix) nil t)
	(goto-char (line-beginning-position))
      (message "Warning: No section '%s' found." section))))

(defun perl-doc--process-links ()
  "Find the next link in a POD section, and process it.
The L<...> syntax is the most complex markup in the POD family of
strange things.  Also, quite a lot of modules on CPAN and
elsewhere found ways to violate the spec in interesting ways
which seem to work, at least, with some formatters."
  ;; Note: Processing links can't be done with syntax tables by using
  ;; <> as a bracket pair because links can contain unbalanced < or >
  ;; symbols.  So do it the hard way....
  (goto-char (point-min))
  ;; Links, in general, have three components: L<text|name/section>.
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
  ;; By the way: Are you tired of backslasheritis?  Well, I am.
  (let* (({  "\\(?:")
	 ({1 "\\(?1:")
	 ({2 "\\(?2:")
	 ({3 "\\(?3:")
	 (}  "\\)")
	 (or "\\|")
	 (bs "\\\\")
	 (q  "\"")
	 (ws	(concat { "[[:blank:]]" or "\n" } ))
	 (quoted    (concat { q { bs bs or bs q or "[^\"]" } "*" q } ))
	 (plain     (concat { "[^|/<>]" } ))
	 (extended  (concat { "[^|/]" } ))
	 (nomarkup  (concat { "[^A-Z]<" } ))
	 (no-del    (concat { bs "|" or bs "/" or "[^|/]" } ))
	 (m2	(concat { "[A-Z]<<" ws no-del "+?" ws ">>" } ))
	 (m0	(concat { "[A-Z]<" { "[^<>|/]" or nomarkup } "+?>" } ))
	 (markup    (concat { m2 or "[A-Z]<"
			    { m2 or m0 or nomarkup or "[^|/>]" }
			    "+?>" } ))
	 (component (concat { plain or markup or nomarkup } ))
	 (name      (concat {2 { "[^ \"\t|/<>]" or markup } "*" } ))
	 (url       (concat {2 "\\w+:/[^ |<>]+" } ))
	 ;; old-style references to a section in the same page.
	 ;; This style is deprecated, but found in the wild.  We are
	 ;; following the recommended heuristic from perlpodspec:
	 ;;    .... if it contains any whitespace, it's a section.
	 ;; We also found quoted things to be sections.
	 (old-sect  (concat {2 { component "+ " component "+" }
			    or quoted
			    }  )))
    (while (re-search-forward (rx "L<" (optional (group-n 1 (1+ "<") " ")))
			      nil t)
      (let* ((terminator-length (length (match-string 1)))
	     (allow-angle (> terminator-length 0)); L<< ... >>
	     (text  (if allow-angle
			(concat {1 extended "+?" } )
		      (concat {1 component "+?" } )))
	     (section (if allow-angle
			  (concat {3 quoted or extended "+?" } )
			(concat {3 quoted or component "+" } )))
	     (terminator (if allow-angle
			     (concat " " (make-string terminator-length ?>))
			   ">"))
	     (link-re   (concat "\\="
				{ { text "|" } "?"
				  {
				    { name { "/" section } "?" }
				    or url or old-sect
				  }
				}))
	     (re	(concat link-re terminator))
	     (end-marker (make-marker)))
	(re-search-forward re nil t)
	(set-marker end-marker (match-end 0))
	(cond
	 ((null (match-string 2))
	  ;; This means that the regexp failed.  Either the L<...>
	  ;; element is really, really bad, or the regexp isn't
	  ;; complicated enough.  Since the consequences are rather
	  ;; harmless, don't raise an error.
	  (message "perl-doc: Unexpected string: %s"
		   (buffer-substring (line-beginning-position)
				     (line-end-position))))
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
	    (and (match-string 1) (string-match quoted (match-string 2))))
	  ;; L<unlink1|"unlink1"> -> L<unlink1|/"unlink1">, as seen in File::Temp
	  (goto-char (match-beginning 2))
	  (insert "/"))
	 ((save-match-data
	    (string-match quoted (match-string 2)))
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

(defun perl-doc (word &optional section)
  "Get Perl documentation like the perldoc command.
Does better formatting than man pages, including hyperlinks."
  (interactive
   (let* ((default (cperl-word-at-point))
	 (read (read-string
		(cperl--format-prompt "Find doc for Perl function" default))))
     (list (if (equal read "")
	      default
	    read))))
  (require 'shr)
  (let* ((case-fold-search nil)
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
	(switch-to-buffer perldoc-buffer)
      (with-temp-buffer
	;; for diagnostics comment out the previous line, and
	;; uncomment the next.  This makes the intermediate buffer
	;; permanent for inspection in the pod- and html-phase.
	;; (with-current-buffer (get-buffer-create (concat "**pod-" word "**"))
	;; Fetch plain POD into a temporary buffer
	(when (< 0 (if is-func
		       (call-process perl-doc-perldoc-program nil t t "-u" "-f" word)
		     (call-process perl-doc-perldoc-program nil t t "-u" word)))
	  (error (buffer-string)))
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
      (put-text-property (point-min) (point-max)
			 'keymap perl-doc-shr-map)
      (if is-func
	  (setq-local perl-doc-base "perlfunc")
	(setq-local perl-doc-base nil))
      (set-buffer-modified-p nil)
      (read-only-mode))
    (when section
      (perl-doc-goto-section section))
    (setq-local revert-buffer-function #'perl-doc--refresh
		perl-doc-current-word word
		perl-doc-current-section section)))

(defun perl-doc--refresh (_ignore-auto _noconfirm)
  "Refresh the current piece of documentation."
  (rename-buffer "*html*" t)
  (let ((inhibit-read-only t))
    (perl-doc perl-doc-current-word perl-doc-current-section)))

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
				"\\(?:\\(?1:.*\\)"   ; 1: page, may be empty
				"\\(?:#\\|/\\)"      ; section separator
				"\\(?2:.+\\)" ; "/" + 2: nonzero section
				"\\|"		; or
				"\\(?1:.+\\)\\)$")   ; 1: just a page
			url)
	  ;; link to be handled by perl-doc
	(let ((page   (match-string 1 url))
	      (section (match-string 2 url)))
	  (if (> (length page) 0)
	      (if (null (string-match "([1-9])$" page))
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

(provide 'perl-doc)
;;; perldoc.el ends here

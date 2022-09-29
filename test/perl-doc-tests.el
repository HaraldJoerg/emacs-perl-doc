;;; perl-doc-tests.el --- Test for perl-doc  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Harald Jörg <haj@posteo.de>
;; Maintainer: Harald Jörg
;; Keywords: languages
;; URL: https://github.com/HaraldJoerg/emacs-perl-doc

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a collection of tests for perl-doc.el

;;; Code:

(require 'perl-doc)
(require 'ert)
(require 'ert-x)

(ert-deftest perl-doc-test-l-grammar ()
  "Tests the individual grammar elements for L<...> POD stuff."
  (perl-doc-with-L-grammar
   (let ((string "\\"))
     (string-match (rx backslash) string)
     (should (string= (match-string 0 string) "\\")))
   ;; 'quoted' must recognize escaped quotes
   (let ((string "text \\ \"quoted \\\"part\\\"\\\\\" more text"))
     (string-match (rx quoted) string)
     (should (string= (match-string 0 string) "\"quoted \\\"part\\\"\\\\\"")))
   (let ((markup-testcases
	  '(("B<bold> xxx" . "B<bold>")
	    ("C<|>" . "C<|>")		; seen in perlport
	    ("I<nestB<ed>>>" . "I<nestB<ed>>")
	    ("C<< extended with | >>>" . "C<< extended with | >>")
	    ("C<< extended with > >>>" . "C<< extended with > >>"))))
     (dolist (markup-testcase markup-testcases)
       (let ((string (car markup-testcase))
	     (match  (cdr markup-testcase)))
	 (should (string-match (rx markup) string))
	 (should (string= (match-string 0 string) match)))))))

(ert-deftest perl-doc-test-process-links ()
  "Test various ways to write POD \"L<...>\" elements.
The L markup is the weirdest of all POD elements, here are some
  examples from real Perl and CPAN modules.  Most examples are
  from perlfunc.pod, with words abbreviated to avoid over-long
  lines."
  (let ((conversions
	 '(("L<perlrun>" .		; plain link to perldoc
	    "L<perlrun|perldoc:///perlrun>")
	   ("L<C<time>|/time>" .	; markup + label + local section
	    "L<C<time>|/time>")
	   ("L<http://www.cpan.org/>" .	; WWW link in perlintro.pod
	    "L<http://www.cpan.org/>")
	   ("L<CPAN|http://www.cpan.org/>" . ; WWW link with label
	    "L<CPAN|http://www.cpan.org/>")
	   ("L<C<trE<sol>E<sol>E<sol>>|/trE<sol>E<sol>E<sol>>" .
	    "L<C<trE<sol>E<sol>E<sol>>|/trE<sol>E<sol>E<sol>>")
	   ("L<C<\"switch\"> f|f/The 'switch' f>" . ; spaces
	    "L<C<\"switch\"> f|perldoc:///f/The-'switch'-f>")
	   ("L<fopen(3)>" . "L<fopen(3)|perldoc:///fopen(3)>")
	   ("L<pi/Files and I/O>" .	; in perlfunc.pod
	    "L<Files and I/O in pi|perldoc:///pi/Files-and-I/O>")
	   ("L<C<^>, C<&> and C<|>|perlop/Bitwise>" . ; found in perlport
	    "L<C<^>, C<&> and C<|>|perldoc:///perlop/Bitwise>")
	   ("L<< Perl-R|https://g.com/orgs/Perl/teams/perl-r >>" .
	    "L<< Perl-R|https://g.com/orgs/Perl/teams/perl-r >>")))
	 (perl-doc--debug t))
    (dolist (test conversions)
      (with-temp-buffer
	(insert (car test))
	(perl-doc--process-links)
	(should (string= (buffer-string) (cdr test)))))))


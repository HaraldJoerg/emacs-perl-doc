# emacs-perl-doc
Read nicely rendered Perl documentation in Emacs

This file contains a command to read Perl documentation in Emacs.
It uses two external commands which come with Perl: `perldoc` to
locate the Perl documentation for the Perl modules installed on
your system, and `pod2html` to format the documentation to HTML.
This HTML version is then displayed using Emacs' "simple HTML
renderer" shr.

## Motivation

Perl documentation is written in a markup format called POD ([Plain
Old Documentation](https://perldoc.perl.org/perlpod) and is usually
converted to other formats for human consumption.  The documentation
used to be available in Emacs for a long time in 'info' or 'man'
format.  However, Perl does no longer ship 'info' files, and the
software available from CPAN never did.  'man' is not available on all
platforms and allows only rather restricted formatting, most notably
linking between documents does not work.

On the other hand, Perl provides a converter from POD to HTML.
HTML is well supported by Emacs and is well suited for presentation
of structured documents.

The user visible benefits over the other formats are:
 * Works nicely on platforms which do not have man
 * Unlike with 'man', Hyperlinks between POD documents work
   and resolve to POD documentation on your system, no web server required.
 * Makes use of Emacs faces: variable-pitch font for text,
   fixed-pitch for code, italics for, well, italics

## Installation and Configuration

This library works with Emacs 28.  More precise, it requires
cperl-mode.el from Emacs 28 which is available from the Emacs
repository:
[cperl-mode.el](https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/cperl-mode.el),
and Emacs 27.

The file perl-doc.el is not (yet) available from any Emacs package
repository.  You can store it somewhere on the 'load-path' of your
Emacs.

The file comes with two customization items
`perl-doc-pod2html-program` and `perl-doc-perldoc-program` which point
to the pod2html and perldoc programs, respectively.  On many platforms
the defaults will just work, though you may need to install a separate
package for Perl documentation (e.g. perl-doc for Debian/Ubuntu).

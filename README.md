# emacs-perl-doc
Read nicely rendered Perl documentation in Emacs

This file contains commands to read Perl documentation in Emacs.
It uses two external commands which come with Perl: `perldoc` to
locate the Perl documentation for the Perl modules installed on
your system, and `pod2html` to format the documentation to HTML.
This HTML version is then displayed using Emacs' "simple HTML
renderer" shr.

  * `perl-doc`: Read perl documentation, prompt for topic.  You can
    give perldoc sections (e.g. "perldebug"), names of modules
    installed on your system, but also functions and variable names.
  * `perl-doc-file`: Like `perl-doc`, but prompts for a file name.
  * `perl-doc-view-source`: View the POD source for the documentation
    in the current buffer.

## Motivation

Perl documentation is written in a markup format called POD ([Plain
Old Documentation](https://perldoc.perl.org/perlpod) and is usually
converted to other formats for reading by humans.  The documentation
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

This library is available under the name perl-doc
from [GNU ELPA](https://elpa.gnu.org/packages/) 
and works with Emacs 27 and newer.  Indexing with imenu can
be used with Emacs 28 and newer.

The file comes with two customization items
`perl-doc-pod2html-program` and `perl-doc-perldoc-program` which point
to the pod2html and perldoc programs, respectively.  On many platforms
the defaults will just work, though you may need to install a separate
package for Perl documentation (e.g. perl-doc for Debian/Ubuntu).

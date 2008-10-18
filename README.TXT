                                oBrowser
                       --------------------------

1. PREREQUISITES
----------------------------------------------------------------------

  To be able  to build oBrowser, you need  several software.
This build dependencies will change in the future to be smaller.

Mandatory to build the core
 - a bourne-like shell (like bash)
 - the GNU version of make
 - perl (this will be removed)
 - OCaml 3.10+ and an associated C compiler

Not mandatory (optional examples)
 - Ocsigen to build and run the ocsigen-related examples
 - Menhir (and ocamllex)

2. GETTING STARTED
----------------------------------------------------------------------

  Just  type "make  tutorial", then  open tutorial/index.html  in your
favorite browser. Not all browsers are fully supported yet.
You can also open examples.html to run the examples.

  We have  tried our  best to make  this software compatible  with all
browsers, but  since it is still in  an early stage, you  may need the
last version of your favorite browser. By now, it has been tested on :

 - Mozilla Firefox 2 and 3 (beta)
 - Opera 9.5 (beta)     
 - Safari 3.1 for Windows and 3.2 for MacOSX
 - Konqueror 4
 - Microsoft Internet Explorer 8 (beta)

The two later ones have  proven slower at running our JavaScript code,
we may be able  to profile our code better in the  future and make the
situation change

  Our main development platform is Mozilla Firefox 3, since the
firebug third-party extension enables the developper to inspect, check
and edit the DOM, CSS and JavaScript data structures live, which may
be a good tool to understand how your code interacts with the DOM too.

3. LINKS
----------------------------------------------------------------------

To download the ocaml distribution :
  http://ocaml.inria.fr/
To download the ocsigen distribution :
  http://www.ocsigen.org/
To install a build environment on Windows, see, for instance, cygwin :
  http://www.cygwin.com/
To download the last version browsers (as of may the 27th 2k8) :
  http://www.microsoft.com/windows/products/winfamily/ie/ie8/getitnow.mspx
  http://www.mozilla.com/en-US/firefox/all-beta.html
  http://www.apple.com/safari/
  http://www.opera.com/products/desktop/next/
  http://www.kde.org/
Check if Konqueror 4 and  Firefox 3b is available in your distribution
before downloading.

Note: If  you run a  recent Debian-based GNU/Linux  distribution (like
Debian unstable or Ubuntu 8.04), all the requirements are available in
the repositories.

4. (FREE SOFTWARE) LICENCES
----------------------------------------------------------------------

We are reusing code for  several separate projects, so there are several
licences (although they are all free software ones).

From OCaml (rt/caml/):
  We only distribute a modified standard library (not the compiler),
  so the sources are distributed under the terms of the GNU Library
  General Public License, with the special exception on linking
  described in file rt/LICENSE
For our own code (rt/js/):
  The JavaScript runtime and virtual machine are distributed under
  the terms of the GNU Library General Public License, with the
  special exception on linking described in file rt/LICENSE
From mozilla (rt/plugin/) :
  Not distributed yet
For the tutorial (tutorial/):
  The tutorial is distributed under the terms of the GNU Free
  Documentation License version 1.2 or newer at your option,
  see tutorial/LICENSE. However, the included examples are
  distributed as explained in the following paragraph.
For the examples (examples/):
  Most of the examples quite trivial code and you are free to use them
  under the terms of the Do What The Fuck You Want Public License.
  Some of them are derived from other examples, see the LICENSE files
  in each example directory.
Pictures (tutorial/pictures, tutorial/thumbs):
  Tje photographs used in the viewer example are personal work, feel
  free to use or redistribute them under the Do What The Fuck You
  Want Public License.

5. ABOUT THE AUTHORS
----------------------------------------------------------------------

The  main author  of this  software is  Benjamin Canou.   It  has been
written as  part of his PhD  thesis experiments, with the  help of his
advisors  Vicent Balat  (see also  the ocsigen  project)  and Emmanuel
Chailloux.

                                     Contact: Benjamin.Canou@gmail.com

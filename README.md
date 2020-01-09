Overview
========

metapost-mode+.el is an extension to old metapost-mode.el in GNU Emacs. It will add following new features to metapost-mode:

![screenshot](http://www.comp.hkbu.edu.hk/~yli/uploads/Code/mp-screenshot.png "Screenshot")


* Compile the .mp file and preview the results smartly by C-c C-c.
* Notify user when the compile is failed and jump to error line with C-c `.
* Support metapost with LaTeX labels.

System Requirement
==================

* GNU Emacs 23.2+ (with corresponding version of metapost-mode.el and doc-view.el)
* Workable TeX/LaTeX installation, including ghostscript/epstopdf
* MacOSX Snow Leopard and Ubuntu are tested, other linux should work. Windows is not tested and probably breaks.

Get metapost-mode+.el
=====================

You can get the lastest version of metapost-mode+.el at github. Documentation is included in the source, or you can find it with discussion on emacswiki.

Usage
=====

* Make sure your metapost, gs and epstopdf work. Try "metapost some.mp" and "epstopdf some.eps" to check it.
* Make sure your doc-view.el is newer than the corresponding version with GNU Emacs 23.2
* Add (require ‘metapost-mode+) to your .emacs
* Open some example in the test dir on github, such as MultiFigure.mp.
* C-c C-c to see the preview figure

Feedback and Suggestion
=======================

Feel free to give me feedback and suggestion on github or emacswiki, or via the email in source.

License
=======

Thanks @johanvts to remind me this. But now it is under MIT license.


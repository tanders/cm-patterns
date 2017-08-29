;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :asdf-user)

(asdf:defsystem cm-patterns
  :description "Rick Taube's 'Pattern Streams' from Common Music v.2 without Common Music. This code is based on the Patterns library by Anders Vinjar, who ported the Common Music to OpenMusic. See https://forge.ircam.fr/p/omlibraries/downloads/639/

This code is ported for Clozure CL (Opusmodus).

A great guide to composing using patterns is chapter 20 in Rick's book (Taube, 2004). Patterns are used everywhere in the book; is a nice intro. The code of the book is available online at http://www.moz.ac.at/sem/lehre/lib/cm/Notes%20from%20the%20Metalevel/00/contents.html  See also the reference documentation at http://commonmusic.sourceforge.net/cm2/doc/dict/index.html

The data parsing features of the original versions: 'keynum', 'transpose' etc. are not provided in this version of the lib.

* References 

Taube, H. (2004) Notes from the Metalevel. London and New York: Taylor & Francis." 
  :author "Rick Taube <taube@illinois.edu> (original design), Anders Vinjar <anders.vinjar@bek.no> (port to OpenMusic), Torsten Anders <torsten.anders@beds.ac.uk> (ASDF library for plain Common Lisp)"
  :licence "GPLv2"
  ;; :licence "Attribution-NonCommercial-ShareAlike Vizsage Public License"
  :version "0.1"
  :serial t ;; the dependencies are linear.
  :components ((:file "sources/make-package")
	       (:file "sources/utils")
	       (:file "sources/pattern-classes")
	       )
  :depends-on ())



#! /usr/bin/gosh
;-*-Scheme-*-

(use www.cgi)
(use text.html-lite)
(use rfc.cookie)
(use util.list)
(use file.util)
(use srfi-1)
(use srfi-13)
(use srfi-19)
(use util.match)

(set! *load-path* (cons "/home/igarashi/git/copl-tools/cgi/" *load-path*))
(load "./site-local")
(use global)
(use userdb)
(use questions)
(use hilight)
(use process)
(use statistics)

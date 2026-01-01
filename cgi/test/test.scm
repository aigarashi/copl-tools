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

; (load "./site-local.scm")
(define-constant *system-dir* "/home/igarashi/git/copl-tools/cgi/test/")
(load "../global.scm")
(load "../userdb.scm")
(load "../questions.scm")
(load "../hilight.scm")
(load "../process.scm")
(load "../statistics.scm")

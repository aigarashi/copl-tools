;; global.scm
;; define global variables

(define *userdb-dir* ;; user database directory
  (string-append *system-dir* "users/"))

(define *question-db* ;; database file for questions
  (string-append *system-dir* "questions.db"))

(define *news*
  (string-append *system-dir* "news.scm"))

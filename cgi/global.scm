;; global.scm
;; define global variables

(define *userdb-dir* ;; user database directory
  (string-append *system-dir* "users/"))

(define *question-db* ;; database file for questions
  (string-append *system-dir* "questions.db"))

(define-constant *domainname* ;; domain name used for cookies
  "localweb.sato.kuis.kyoto-u.ac.jp")

(define-constant *passwd* ;; user-password assoc list
  '(("igarashi" . "Iwv9xWu5eDmQw")

    ))

(define-constant *style* "
  span.error {
    background-color : #ffcccc;
  }
  .qlist {
    border-collapse: collapse;
    border: 1px #1c79C6 solid;
  }  
  .qlist td {
    border: 1px #1c79C6 solid;
    width: 20px;
    text-align: center;
  }
")


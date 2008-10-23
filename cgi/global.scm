;; global.scm
;; define global variables

(define *userdb-dir* ;; user database directory
  (string-append *system-dir* "users/"))

(define *question-db* ;; database file for questions
  (string-append *system-dir* "questions.db"))

(define-constant *passwd* ;; user-password assoc list
  '(("igarashi" . "Iwv9xWu5eDmQw")
    ("ina" . "4ytLrJrNWyG4c")    ;; °ËÆà ÎÓÂÀÏº 6930-20-3977
    ("horii" . "l3FgZF.OL7ANk")  ;; ËÙ°æ Íª 6930-20-9390
    ("kuno" . "0cevKT.dMghoo")   ;; µ×Ìî ¿µÌï
    ("phirose" . "B7.qv3RYpuOus") ;; ×¢À¥ ´îµ¬
    ("nishikawa" . "1sQbWLCXiNbrc") ;; À¾Àî ÆÁ¹¨ (¥·¥¹¥Æ¥à²Ê³Ø)
    ("h-kojima" . "z8ngwSkhknlCg") ;; ¾®Åç ·¼»Ë (ÄÌ¿®¾ğÊó¥·¥¹¥Æ¥à)
    ("hiroki.otaka" . "Qa76UxHZW8IKM") ;; Èø¹â ¹°µ®
    ("kounohi" . "AREVbfGeRrffA") ;; ²ÏÌîÍÎ»Ö 6930-20-1883
    ("yamazoe" . "meM84crFtGceE") ;; »³Åº Hiroaki
    ("k_nakata" . "C8253tgerZDLQ") ;; ÃæÅÄ Kensuke
    ("nguyen_anh" . "xPpZ5cu4ivzGw") ;; ¥°¥§¥ó¥¢¥ó¥Ç¥£¥ó
    ("m.ikeda" . "cPc7pMJFz0iis")  ;; ÃÓÅÄ ¿¿ÅÚÎ¤
    ("yyoshida" . "lP/R0Pu3vciuM") ;; µÈÅÄ Íª°ì 6930-19-7421
    ("tofushiku" . "5j.NKL6QZx4So") ;; Ê¡¸¶ ÉÒ¹Ô
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


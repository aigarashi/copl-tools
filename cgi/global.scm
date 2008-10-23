;; global.scm
;; define global variables

(define *userdb-dir* ;; user database directory
  (string-append *system-dir* "users/"))

(define *question-db* ;; database file for questions
  (string-append *system-dir* "questions.db"))

(define-constant *passwd* ;; user-password assoc list
  '(("igarashi" . "Iwv9xWu5eDmQw")
    ("ina" . "4ytLrJrNWyG4c")    ;; ���� ����Ϻ 6930-20-3977
    ("horii" . "l3FgZF.OL7ANk")  ;; �ٰ� ͪ 6930-20-9390
    ("kuno" . "0cevKT.dMghoo")   ;; ���� ����
    ("phirose" . "B7.qv3RYpuOus") ;; ע�� �
    ("nishikawa" . "1sQbWLCXiNbrc") ;; ���� ���� (�����ƥ�ʳ�)
    ("h-kojima" . "z8ngwSkhknlCg") ;; ���� ���� (�̿����󥷥��ƥ�)
    ("hiroki.otaka" . "Qa76UxHZW8IKM") ;; ���� ����
    ("kounohi" . "AREVbfGeRrffA") ;; �����λ� 6930-20-1883
    ("yamazoe" . "meM84crFtGceE") ;; ��ź Hiroaki
    ("k_nakata" . "C8253tgerZDLQ") ;; ���� Kensuke
    ("nguyen_anh" . "xPpZ5cu4ivzGw") ;; �����󥢥�ǥ���
    ("m.ikeda" . "cPc7pMJFz0iis")  ;; ���� ����Τ
    ("yyoshida" . "lP/R0Pu3vciuM") ;; ���� ͪ�� 6930-19-7421
    ("tofushiku" . "5j.NKL6QZx4So") ;; ʡ�� �ҹ�
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


type token =
  | EOF
  | BY
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | SEMI
  | ID of (Derivation.rulename)
  | LCID of (string)
  | INTL of (int)
  | PLUS
  | EVALTO
  | MINUS
  | MULT
  | IS
  | LESS
  | THAN
  | NOT
  | AST
  | CROSS
  | HYPHEN
  | LT
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | VDASH
  | COMMA
  | LET
  | EQ
  | IN
  | FUN
  | RARROW
  | QM

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Core.judgment Derivation.t
val partialj :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Core.in_judgment

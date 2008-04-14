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

open Parsing;;
# 2 "parser.mly"
open Core
open Derivation

let errBtw i j s =
  MySupport.Error.errBtw 
    (Parsing.rhs_start_pos i) (Parsing.rhs_end_pos j) s

let errAt i s =
  MySupport.Error.errAt (Parsing.rhs_start_pos i) s
# 52 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* BY *);
  258 (* LBRACE *);
  259 (* RBRACE *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* SEMI *);
  268 (* PLUS *);
  269 (* EVALTO *);
  270 (* MINUS *);
  271 (* MULT *);
  272 (* IS *);
  273 (* LESS *);
  274 (* THAN *);
  275 (* NOT *);
  276 (* AST *);
  277 (* CROSS *);
  278 (* HYPHEN *);
  279 (* LT *);
  280 (* IF *);
  281 (* THEN *);
  282 (* ELSE *);
  283 (* TRUE *);
  284 (* FALSE *);
  285 (* VDASH *);
  286 (* COMMA *);
  287 (* LET *);
  288 (* EQ *);
  289 (* IN *);
  290 (* FUN *);
  291 (* RARROW *);
  292 (* QM *);
    0|]

let yytransl_block = [|
  265 (* ID *);
  266 (* LCID *);
  267 (* INTL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\005\000\
\005\000\006\000\006\000\006\000\006\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\007\000\007\000\010\000\
\010\000\008\000\008\000\008\000\008\000\008\000\011\000\011\000\
\011\000\012\000\012\000\014\000\014\000\016\000\016\000\018\000\
\018\000\013\000\015\000\015\000\017\000\020\000\020\000\019\000\
\019\000\019\000\019\000\019\000\019\000\009\000\009\000\009\000\
\009\000\009\000\000\000\000\000"

let yylen = "\002\000\
\001\000\001\000\005\000\005\000\002\000\004\000\005\000\001\000\
\001\000\002\000\003\000\003\000\002\000\005\000\005\000\005\000\
\005\000\005\000\006\000\004\000\005\000\003\000\004\000\005\000\
\003\000\004\000\005\000\003\000\004\000\005\000\005\000\005\000\
\005\000\005\000\004\000\005\000\003\000\004\000\005\000\003\000\
\004\000\005\000\003\000\004\000\005\000\000\000\004\000\000\000\
\005\000\001\000\001\000\003\000\003\000\003\000\006\000\006\000\
\004\000\003\000\001\000\003\000\001\000\003\000\001\000\002\000\
\001\000\001\000\001\000\001\000\001\000\002\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\001\000\002\000\001\000\
\001\000\009\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\002\000\000\000\083\000\001\000\000\000\
\000\000\000\000\000\000\084\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\028\000\000\000\025\000\000\000\000\000\
\000\000\008\000\009\000\000\000\000\000\075\000\072\000\000\000\
\000\000\073\000\074\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\000\000\071\000\065\000\000\000\037\000\000\000\
\043\000\000\000\040\000\000\000\000\000\023\000\000\000\029\000\
\000\000\026\000\000\000\000\000\000\000\006\000\000\000\000\000\
\070\000\000\000\000\000\000\000\020\000\000\000\066\000\000\000\
\067\000\068\000\000\000\069\000\000\000\064\000\000\000\078\000\
\000\000\080\000\081\000\000\000\038\000\000\000\044\000\000\000\
\041\000\000\000\035\000\000\000\024\000\015\000\030\000\017\000\
\027\000\016\000\018\000\000\000\007\000\003\000\000\000\004\000\
\077\000\076\000\000\000\000\000\000\000\021\000\014\000\052\000\
\000\000\000\000\053\000\000\000\054\000\000\000\000\000\079\000\
\049\000\039\000\032\000\045\000\034\000\042\000\033\000\036\000\
\031\000\019\000\013\000\010\000\000\000\000\000\000\000\057\000\
\000\000\000\000\000\000\011\000\012\000\000\000\000\000\000\000\
\055\000\056\000\000\000\000\000\000\000\000\000\082\000"

let yydgoto = "\003\000\
\006\000\012\000\111\000\008\000\036\000\112\000\009\000\046\000\
\092\000\010\000\047\000\048\000\080\000\049\000\083\000\050\000\
\146\000\051\000\052\000\053\000"

let yysindex = "\156\000\
\001\000\002\255\000\000\000\000\180\255\000\000\000\000\177\255\
\249\254\077\255\188\255\000\000\078\255\018\255\053\255\103\255\
\042\255\000\000\195\255\142\255\092\255\116\255\138\255\143\255\
\142\255\000\000\015\255\000\000\060\255\000\000\063\255\099\255\
\120\255\000\000\000\000\026\255\142\255\000\000\000\000\139\255\
\142\255\000\000\000\000\149\255\170\255\011\255\000\000\164\255\
\185\255\189\255\101\255\000\000\000\000\171\255\000\000\065\255\
\000\000\067\255\000\000\084\255\020\255\000\000\144\255\000\000\
\145\255\000\000\151\255\186\255\183\255\000\000\136\255\181\255\
\000\000\187\255\178\255\173\255\000\000\163\255\000\000\142\255\
\000\000\000\000\142\255\000\000\142\255\000\000\000\000\000\000\
\200\255\000\000\000\000\184\255\000\000\007\255\000\000\008\255\
\000\000\009\255\000\000\010\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\202\255\000\000\000\000\085\255\000\000\
\000\000\000\000\142\255\142\255\142\255\000\000\000\000\000\000\
\185\255\189\255\000\000\189\255\000\000\101\255\210\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\098\255\190\255\191\255\000\000\
\161\255\161\255\211\255\000\000\000\000\142\255\142\255\192\255\
\000\000\000\000\208\255\193\255\142\255\212\255\000\000"

let yyrindex = "\000\000\
\013\255\013\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\135\255\
\025\255\073\255\014\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\013\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\174\255\000\000\
\000\000\000\000\000\000\012\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\108\255\073\255\000\000\097\255\000\000\049\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\219\000\000\000\000\000\080\000\002\000\231\255\
\144\000\000\000\228\255\000\000\000\000\143\000\104\000\179\255\
\177\000\176\255\207\255\000\000"

let yytablesize = 268
let yytable = "\061\000\
\004\000\086\000\122\000\013\000\126\000\124\000\130\000\132\000\
\134\000\136\000\077\000\072\000\011\000\063\000\062\000\074\000\
\047\000\026\000\063\000\099\000\063\000\020\000\048\000\078\000\
\059\000\070\000\063\000\071\000\027\000\059\000\063\000\059\000\
\100\000\063\000\063\000\063\000\063\000\059\000\063\000\063\000\
\047\000\046\000\131\000\133\000\135\000\137\000\063\000\059\000\
\062\000\059\000\059\000\120\000\028\000\062\000\123\000\062\000\
\125\000\059\000\032\000\064\000\033\000\062\000\066\000\029\000\
\093\000\126\000\095\000\124\000\062\000\062\000\062\000\062\000\
\061\000\062\000\062\000\065\000\086\000\061\000\067\000\061\000\
\094\000\062\000\096\000\097\000\139\000\061\000\021\000\140\000\
\127\000\142\000\143\000\144\000\141\000\061\000\061\000\061\000\
\060\000\061\000\061\000\098\000\148\000\060\000\030\000\060\000\
\037\000\061\000\025\000\058\000\005\000\060\000\038\000\039\000\
\058\000\031\000\058\000\055\000\068\000\060\000\060\000\060\000\
\058\000\060\000\060\000\054\000\153\000\154\000\056\000\042\000\
\043\000\060\000\058\000\158\000\058\000\058\000\051\000\109\000\
\069\000\057\000\110\000\051\000\058\000\051\000\059\000\101\000\
\103\000\037\000\005\000\051\000\058\000\073\000\105\000\038\000\
\039\000\060\000\102\000\104\000\001\000\002\000\075\000\051\000\
\051\000\106\000\118\000\040\000\037\000\041\000\087\000\051\000\
\042\000\043\000\038\000\039\000\044\000\088\000\087\000\045\000\
\018\000\019\000\046\000\076\000\113\000\088\000\040\000\048\000\
\089\000\114\000\079\000\042\000\043\000\090\000\091\000\014\000\
\089\000\015\000\016\000\017\000\107\000\090\000\091\000\022\000\
\108\000\023\000\024\000\034\000\035\000\081\000\082\000\117\000\
\084\000\116\000\128\000\115\000\138\000\129\000\147\000\150\000\
\152\000\156\000\159\000\007\000\149\000\119\000\121\000\151\000\
\145\000\155\000\085\000\157\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000"

let yycheck = "\025\000\
\000\000\051\000\080\000\002\000\085\000\083\000\000\001\000\001\
\000\001\000\001\000\001\037\000\011\001\000\001\000\001\041\000\
\005\001\000\001\005\001\000\001\007\001\029\001\010\001\013\001\
\000\001\000\001\013\001\002\001\011\001\005\001\016\001\007\001\
\013\001\020\001\021\001\022\001\023\001\013\001\025\001\026\001\
\029\001\029\001\036\001\036\001\036\001\036\001\033\001\023\001\
\000\001\025\001\026\001\080\000\000\001\005\001\083\000\007\001\
\085\000\033\001\017\001\000\001\019\001\013\001\000\001\011\001\
\000\001\146\000\000\001\145\000\020\001\021\001\022\001\023\001\
\000\001\025\001\026\001\016\001\126\000\005\001\016\001\007\001\
\016\001\033\001\016\001\000\001\000\001\013\001\010\001\003\001\
\087\000\115\000\116\000\117\000\008\001\021\001\022\001\023\001\
\000\001\025\001\026\001\016\001\003\001\005\001\000\001\007\001\
\004\001\033\001\029\001\000\001\011\001\013\001\010\001\011\001\
\005\001\011\001\007\001\000\001\018\001\021\001\022\001\023\001\
\013\001\025\001\026\001\032\001\150\000\151\000\011\001\027\001\
\028\001\033\001\023\001\157\000\025\001\026\001\000\001\000\001\
\017\001\000\001\003\001\005\001\033\001\007\001\000\001\000\001\
\000\001\004\001\011\001\013\001\011\001\011\001\000\001\010\001\
\011\001\011\001\011\001\011\001\001\000\002\000\010\001\025\001\
\026\001\011\001\000\001\022\001\004\001\024\001\004\001\033\001\
\027\001\028\001\010\001\011\001\031\001\011\001\004\001\034\001\
\000\001\001\001\005\001\010\001\000\001\011\001\022\001\010\001\
\022\001\005\001\023\001\027\001\028\001\027\001\028\001\012\001\
\022\001\014\001\015\001\016\001\011\001\027\001\028\001\012\001\
\018\001\014\001\015\001\009\001\010\001\021\001\022\001\035\001\
\020\001\032\001\011\001\025\001\011\001\030\001\005\001\026\001\
\006\001\010\001\007\001\001\000\141\000\078\000\080\000\033\001\
\121\000\034\001\050\000\035\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\011\001"

let yynames_const = "\
  EOF\000\
  BY\000\
  LBRACE\000\
  RBRACE\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  SEMI\000\
  PLUS\000\
  EVALTO\000\
  MINUS\000\
  MULT\000\
  IS\000\
  LESS\000\
  THAN\000\
  NOT\000\
  AST\000\
  CROSS\000\
  HYPHEN\000\
  LT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  VDASH\000\
  COMMA\000\
  LET\000\
  EQ\000\
  IN\000\
  FUN\000\
  RARROW\000\
  QM\000\
  "

let yynames_block = "\
  ID\000\
  LCID\000\
  INTL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Derivation) in
    Obj.repr(
# 41 "parser.mly"
               ( _1 )
# 321 "parser.ml"
               : Core.judgment Derivation.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
        ( exit 0 )
# 327 "parser.ml"
               : Core.judgment Derivation.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Judgment) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'RName) in
    Obj.repr(
# 46 "parser.mly"
    ( {conc = _1; by = _3; since = []; pos = rhs_start_pos 3 } )
# 335 "parser.ml"
               : 'Derivation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Judgment) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'RName) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Derivs) in
    Obj.repr(
# 48 "parser.mly"
    ( {conc = _1; by = _3; since = _5; pos = rhs_start_pos 3 } )
# 344 "parser.ml"
               : 'Derivation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Judgment) in
    Obj.repr(
# 49 "parser.mly"
                   ( errAt 2 "Syntax error: \"by\" expected after a judgment" )
# 351 "parser.ml"
               : 'Derivation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'Judgment) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'RName) in
    Obj.repr(
# 50 "parser.mly"
                            ( errAt 4 "Syntax error: opening brace expected" )
# 359 "parser.ml"
               : 'Derivation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Judgment) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'RName) in
    Obj.repr(
# 51 "parser.mly"
                                   ( errBtw 4 5 "Syntax error: unmatched brace" )
# 367 "parser.ml"
               : 'Derivation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Derivation.rulename) in
    Obj.repr(
# 54 "parser.mly"
       ( _1 )
# 374 "parser.ml"
               : 'RName))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
         ( _1 )
# 381 "parser.ml"
               : 'RName))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Derivation) in
    Obj.repr(
# 58 "parser.mly"
                      ( [ _1 ] )
# 388 "parser.ml"
               : 'Derivs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Derivation) in
    Obj.repr(
# 59 "parser.mly"
                           ( [ _1 ] )
# 395 "parser.ml"
               : 'Derivs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Derivation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Derivs) in
    Obj.repr(
# 60 "parser.mly"
                           ( _1::_3 )
# 403 "parser.ml"
               : 'Derivs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Derivation) in
    Obj.repr(
# 61 "parser.mly"
                     ( errAt 2 "Syntax error: unmatched brace, or semicolon forgotten?" )
# 410 "parser.ml"
               : 'Derivs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Env) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'Val) in
    Obj.repr(
# 64 "parser.mly"
                             ( EvalTo(_1, _3, _5) )
# 419 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "parser.mly"
                           ( AppBOp(Plus, Value_of_int _1, Value_of_int _3, Value_of_int _5) )
# 428 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 66 "parser.mly"
                           ( AppBOp(Mult, Value_of_int _1, Value_of_int _3, Value_of_int _5) )
# 437 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
                            ( AppBOp(Minus, Value_of_int _1, Value_of_int _3, Value_of_int _5) )
# 446 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 68 "parser.mly"
                           ( AppBOp(Lt, Value_of_int _1, Value_of_int _5, Value_of_Boolean True) )
# 454 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 69 "parser.mly"
                               ( AppBOp(Lt, Value_of_int _1, Value_of_int _6, Value_of_Boolean False) )
# 462 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'Env) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Exp) in
    Obj.repr(
# 71 "parser.mly"
                        ( errAt 4 "Syntax error: 'evalto' expected" )
# 470 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Env) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Exp) in
    Obj.repr(
# 72 "parser.mly"
                               ( errAt 5 "Syntax error: value expected" )
# 478 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 73 "parser.mly"
                    ( errAt 3 "Syntax error: natural number expected" )
# 485 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 74 "parser.mly"
                         ( errAt 4 "Syntax error: 'is' expected" )
# 493 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 75 "parser.mly"
                            ( errAt 5 "Syntax error: natural number expected" )
# 501 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 76 "parser.mly"
                    ( errAt 3 "Syntax error: natural number expected" )
# 508 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 77 "parser.mly"
                         ( errAt 4 "Syntax error: 'is' expected" )
# 516 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 78 "parser.mly"
                            ( errAt 5 "Syntax error: natural number expected" )
# 524 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 79 "parser.mly"
                     ( errAt 3 "Syntax error: natural number expected" )
# 531 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 80 "parser.mly"
                          ( errAt 4 "Syntax error: 'is' expected" )
# 539 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 81 "parser.mly"
                             ( errAt 5 "Syntax error: natural number expected" )
# 547 "parser.ml"
               : 'Judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Env) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Exp) in
    Obj.repr(
# 84 "parser.mly"
                            ( In_EvalTo(_1, _3) )
# 555 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 85 "parser.mly"
                         ( In_AppBOp(Plus, Value_of_int _1, Value_of_int _3) )
# 563 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 86 "parser.mly"
                         ( In_AppBOp(Mult, Value_of_int _1, Value_of_int _3) )
# 571 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 87 "parser.mly"
                          ( In_AppBOp(Minus, Value_of_int _1, Value_of_int _3) )
# 579 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'Env) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'Exp) in
    Obj.repr(
# 91 "parser.mly"
                        ( errAt 4 "Syntax error: 'evalto' expected" )
# 587 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Env) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'Exp) in
    Obj.repr(
# 92 "parser.mly"
                               ( errAt 5 "Syntax error: '?' expected" )
# 595 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 93 "parser.mly"
                    ( errAt 3 "Syntax error: natural number expected" )
# 602 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 94 "parser.mly"
                         ( errAt 4 "Syntax error: \'is\' expected" )
# 610 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 95 "parser.mly"
                            ( errAt 5 "Syntax error: '?' expected" )
# 618 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 96 "parser.mly"
                    ( errAt 3 "Syntax error: natural number expected" )
# 625 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 97 "parser.mly"
                         ( errAt 4 "Syntax error: \'is\' expected" )
# 633 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 98 "parser.mly"
                            ( errAt 5 "Syntax error: '?' expected" )
# 641 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 99 "parser.mly"
                     ( errAt 3 "Syntax error: natural number expected" )
# 648 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 100 "parser.mly"
                          ( errAt 4 "Syntax error: \'is\' expected" )
# 656 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 101 "parser.mly"
                             ( errAt 5 "Syntax error: '?' expected" )
# 664 "parser.ml"
               : Core.in_judgment))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
                ( Empty )
# 670 "parser.ml"
               : 'Env))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'Env2) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Val) in
    Obj.repr(
# 105 "parser.mly"
                     ( Bind(_1, _2, _4) )
# 679 "parser.ml"
               : 'Env))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                ( Empty )
# 685 "parser.ml"
               : 'Env2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'Env2) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Val) in
    Obj.repr(
# 109 "parser.mly"
                           ( Bind(_1, _2, _4) )
# 694 "parser.ml"
               : 'Env2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'LongExp) in
    Obj.repr(
# 112 "parser.mly"
            ( _1 )
# 701 "parser.ml"
               : 'Exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Exp1) in
    Obj.repr(
# 113 "parser.mly"
         ( _1 )
# 708 "parser.ml"
               : 'Exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Exp1) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'BinOp1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'LongExp) in
    Obj.repr(
# 114 "parser.mly"
                        ( BinOp(_2, _1, _3) )
# 717 "parser.ml"
               : 'Exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Exp2) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'BinOp2) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'LongExp) in
    Obj.repr(
# 115 "parser.mly"
                        ( BinOp(_2, _1, _3) )
# 726 "parser.ml"
               : 'Exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Exp3) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'BinOp3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'LongExp) in
    Obj.repr(
# 116 "parser.mly"
                        ( BinOp(_2, _1, _3) )
# 735 "parser.ml"
               : 'Exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Exp) in
    Obj.repr(
# 119 "parser.mly"
                             ( If(_2, _4, _6) )
# 744 "parser.ml"
               : 'LongExp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Exp) in
    Obj.repr(
# 120 "parser.mly"
                           ( Let(_2, _4, _6) )
# 753 "parser.ml"
               : 'LongExp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Exp) in
    Obj.repr(
# 121 "parser.mly"
                        ( Abs(_2, _4) )
# 761 "parser.ml"
               : 'LongExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Exp1) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'BinOp1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Exp2) in
    Obj.repr(
# 124 "parser.mly"
                     ( BinOp(_2, _1, _3) )
# 770 "parser.ml"
               : 'Exp1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Exp2) in
    Obj.repr(
# 125 "parser.mly"
         ( _1 )
# 777 "parser.ml"
               : 'Exp1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Exp2) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'BinOp2) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Exp3) in
    Obj.repr(
# 128 "parser.mly"
                     ( BinOp(_2, _1, _3) )
# 786 "parser.ml"
               : 'Exp2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Exp3) in
    Obj.repr(
# 129 "parser.mly"
         ( _1 )
# 793 "parser.ml"
               : 'Exp2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Exp3) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'BinOp3) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Exp4) in
    Obj.repr(
# 132 "parser.mly"
                     ( BinOp(_2, _1, _3) )
# 802 "parser.ml"
               : 'Exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Exp4) in
    Obj.repr(
# 133 "parser.mly"
         ( _1 )
# 809 "parser.ml"
               : 'Exp3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Exp4) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AExp) in
    Obj.repr(
# 137 "parser.mly"
              ( App(_1, _2) )
# 817 "parser.ml"
               : 'Exp4))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'MinExp) in
    Obj.repr(
# 138 "parser.mly"
           ( _1 )
# 824 "parser.ml"
               : 'Exp4))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
       ( Lt )
# 830 "parser.ml"
               : 'BinOp1))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
          ( Plus )
# 836 "parser.ml"
               : 'BinOp2))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "parser.mly"
           ( Minus )
# 842 "parser.ml"
               : 'BinOp2))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "parser.mly"
        ( Mult )
# 848 "parser.ml"
               : 'BinOp3))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 151 "parser.mly"
                ( Exp_of_int (- _2) )
# 855 "parser.ml"
               : 'MinExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AExp) in
    Obj.repr(
# 152 "parser.mly"
         ( _1 )
# 862 "parser.ml"
               : 'MinExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 155 "parser.mly"
         ( Exp_of_int _1 )
# 869 "parser.ml"
               : 'AExp))
; (fun __caml_parser_env ->
    Obj.repr(
# 156 "parser.mly"
         ( Exp_of_Boolean True )
# 875 "parser.ml"
               : 'AExp))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
          ( Exp_of_Boolean False )
# 881 "parser.ml"
               : 'AExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 158 "parser.mly"
         ( Exp_of_string _1 )
# 888 "parser.ml"
               : 'AExp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Exp) in
    Obj.repr(
# 159 "parser.mly"
                      ( _2 )
# 895 "parser.ml"
               : 'AExp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Exp) in
    Obj.repr(
# 160 "parser.mly"
                     ( errBtw 1 3 "Syntax error: unmatched parenthesis" )
# 902 "parser.ml"
               : 'AExp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 163 "parser.mly"
         ( Value_of_int _1 )
# 909 "parser.ml"
               : 'Val))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 164 "parser.mly"
                ( Value_of_int (- _2) )
# 916 "parser.ml"
               : 'Val))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "parser.mly"
         ( Value_of_Boolean True )
# 922 "parser.ml"
               : 'Val))
; (fun __caml_parser_env ->
    Obj.repr(
# 166 "parser.mly"
          ( Value_of_Boolean False )
# 928 "parser.ml"
               : 'Val))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'Env) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'Exp) in
    Obj.repr(
# 167 "parser.mly"
                                                            ( Fun(_2, _6, _8) )
# 937 "parser.ml"
               : 'Val))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry partialj *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Core.judgment Derivation.t)
let partialj (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Core.in_judgment)

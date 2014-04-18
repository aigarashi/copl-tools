open Format
open Core

let pr = fprintf

let print_judgment ppf = function
    AEvalTo(st, a, i) -> pr ppf "%d" i
  | BEvalTo(st, a, b) -> pr ppf "%b" b

let print_pjudgment ppf j = pr ppf "Pretty printer not implemented"

let tex_judgment ppf j = pr ppf "Pretty printer not implemented"


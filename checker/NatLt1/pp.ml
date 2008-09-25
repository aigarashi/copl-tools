open Format
open Core

let g = "NatLti"

let pr = fprintf

let rec print_nat ppf = function
    Z -> pr ppf "Z"
  | S n -> pr ppf "S(%a)" print_nat n

let print_judgment ppf = function
    Lt (n1, n2) -> pr ppf "%a is less than %a" print_nat n1 print_nat n2

let print_pjudgment ppf = function
    In_Lt (n1, n2) -> pr ppf "%a is less than %a" print_nat n1 print_nat n2

let rec tex_nat ppf = function
    Z -> pr ppf "\\%sZTerm" g
  | S n -> pr ppf "\\%sSTerm{%a}" g tex_nat n

let tex_judgment ppf = function
    Lt (n1, n2) -> pr ppf "\\%sLt{%a}{%a}" g tex_nat n1 tex_nat n2



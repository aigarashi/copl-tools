open Format
open Core

let g = "Nat"

let pr = fprintf

let rec print_nat ppf = function
    Z -> pr ppf "Z"
  | S n -> pr ppf "S(%a)" print_nat n

let print_judgment ppf = function
    PlusIs (n1, n2, n3) -> 
      pr ppf "%a plus %a is %a" print_nat n1 print_nat n2 print_nat n3
  | MultIs (n1, n2, n3) ->
      pr ppf "%a times %a is %a" print_nat n1 print_nat n2 print_nat n3

let print_pjudgment ppf = function
    In_PlusIs (n1, n2) -> 
      pr ppf "%a plus %a is ?" print_nat n1 print_nat n2
  | In_MultIs (n1, n2) ->
      pr ppf "%a times %a is ?" print_nat n1 print_nat n2

let rec tex_nat ppf = function
    Z -> pr ppf "\\%sZTerm" g
  | S n -> pr ppf "\\%sSTerm{%a}" g tex_nat n

let tex_judgment ppf = function
    PlusIs (n1, n2, n3) -> 
      pr ppf "\\%sPlusIs{%a}{%a}{%a}" g tex_nat n1 tex_nat n2 tex_nat n3
  | MultIs (n1, n2, n3) ->
      pr ppf "\\%sMultIs{%a}{%a}{%a}" g tex_nat n1 tex_nat n2 tex_nat n3


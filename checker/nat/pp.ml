open Format
open Core

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

let tex_judgment ppf = function
    PlusIs (n1, n2, n3) -> 
      pr ppf "\\PlusIs{%a}{%a}{%a}" print_nat n1 print_nat n2 print_nat n3
  | MultIs (n1, n2, n3) ->
      pr ppf "\\MultIs{%a}{%a}{%a}" print_nat n1 print_nat n2 print_nat n3


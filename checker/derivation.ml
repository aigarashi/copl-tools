open Format
open MySupport.Error

type rulename = string

type 'j t = {
  conc:  'j;
  by:    rulename option;
  since: 'j t list;
  pos:   pos * pos
}

let rec print_deriv pr_j ppf d =
  fprintf ppf "@[<v>@[<v 2>";
  (match d.by with
      Some rn -> fprintf ppf   "@[@[%a@] by %s {@]" pr_j d.conc rn
    | None -> fprintf ppf   "@[@[%a@];\ @]" pr_j d.conc);
  print_derivs pr_j ppf d.since;

and print_derivs pr_j ppf = function
    [] -> fprintf ppf "}@]@]"
  | d :: [] -> fprintf ppf "@,%a@]@,}@]" (print_deriv pr_j) d
  | d :: rest ->
      fprintf ppf "@,%a;%a"
	(print_deriv pr_j) d
	(print_derivs pr_j) rest



let rec tex_deriv tex_j ppf d =
  fprintf ppf "@[@[<v 2>";
  fprintf ppf "@[\\infer[\\mbox{\\textsc{\\scriptsize %s}}]{@[%a@]}{@]"
    (match d.by with Some s -> s | None -> "???") tex_j d.conc;
  tex_derivs tex_j ppf d.since;

and tex_derivs tex_j ppf = function
    [] -> fprintf ppf "}@]@]"
  | d :: [] -> fprintf ppf "@,%a@]@,}@]" (tex_deriv tex_j) d
  | d :: rest ->
      fprintf ppf "@,%a &%a"
	(tex_deriv tex_j) d
	(tex_derivs tex_j) rest

let tex_deriv tex_j ppf d =
  fprintf ppf "\\[@\n";
  tex_deriv tex_j ppf d;
  fprintf ppf "@\n\\]"

open Format
open MySupport.Error

type rulename = string

type 'j derivation = {
  conc:  'j;
  by:    rulename;
  since: 'j derivation list;
  pos:   pos
}

let rec print_deriv pr_j ppf d =
  fprintf ppf "@[<v 2>";
  fprintf ppf "@[@[%a@] by %s {@]" pr_j d.conc d.by;
  print_derivs pr_j ppf d.since;

and print_derivs pr_j ppf = function
    [] -> fprintf ppf "}@]"
  | d :: [] -> fprintf ppf "@,%a@,}@]" (print_deriv pr_j) d
  | d :: rest -> 
      fprintf ppf "@,%a;%a" 
	(print_deriv pr_j) d 
	(print_derivs pr_j) rest
  
  
  

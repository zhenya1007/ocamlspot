open Longident

let rec to_string = function
  | Lident s -> s
  | Ldot (t, s) -> to_string t ^ "." ^ s
  | Lapply (t1, t2) -> Printf.sprintf "%s(%s)" (to_string t1) (to_string t2)



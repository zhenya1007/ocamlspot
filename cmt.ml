open Utils
open Cmt_format

let source_path file = match file.cmt_sourcefile with 
    | Some f -> Some (file.cmt_builddir ^/ f)
    | None -> None

(* xxx.{ml,cmo,cmx,spot} => xxx.spot 
   xxx.{mli,cmi,spit} => xxx.spit *)
let of_path path =
  let dirname, filename =
    try
      let slash = String.rindex path '/' in
      Some (String.sub path 0 slash),
      String.sub path (slash + 1) (String.length path - slash - 1)
    with
    | Not_found -> None, path
  in
  let filename =
    match Filename.split_extension filename with
    | body, (".cmi" | ".mli" | ".cmti") -> body ^ ".cmti"
    | body, _ -> body ^ ".cmt"
  in
  match dirname with
  | None -> filename
  | Some d -> d ^/ filename

(* CR jfuruse: this is a dirty workaround. It should be nice if we could know cmt is created by opt or byte *)          
let is_opt cmt = 
  List.exists (fun x -> match Filename.split_extension x with (_, ".cmx") -> true | _ -> false) (Array.to_list cmt.cmt_args)

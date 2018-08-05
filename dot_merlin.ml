open Utils

type t = 
  | B of string
  | S of string
  | FLG of string
  | Other of string * string

let load fp =
  let ic = open_in fp in
  let rec f st = 
    match input_line ic with
    | exception End_of_file -> List.rev st
    | l ->
        let x = 
          match String.index l ' ' with
          | exception Not_found -> Other (l, "")
          | pos -> 
              let h = String.sub l 0 pos in
              let t = String.(sub l (pos+1) (length l - pos - 1)) in
              match h with
              | "B" -> B t
              | "S" -> S t
              | "FLG" -> FLG t
              | _ -> Other (h, t)
        in
        f @@ x :: st
  in
  let xs = f [] in
  close_in ic;
  xs

let load_path_of_dir dir =
  let dot_merlin = Filename.concat dir ".merlin" in
  if Sys.file_exists dot_merlin then
    Some (List.filter_map (function
        | B x -> Some x
        | _ -> None) & load dot_merlin)
  else None

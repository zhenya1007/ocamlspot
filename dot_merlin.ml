open Utils

type t = 
  | B of string
  | S of string
  | FLG of string
  | Other of string * string

let get_opened_modules_in_FLG s =
  (* XXX escaping? *)
  let tokens = String.split (function ' ' -> true | _ -> false) s in
  let rec get_opened_modules = function
    | "-open" :: x :: xs -> x :: get_opened_modules xs
    | _ :: xs -> get_opened_modules xs
    | [] -> []
  in
  get_opened_modules tokens

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
  let flg = 
    let rec find_FLG = function
      | [] -> []
      | FLG x :: xs -> x :: find_FLG xs
      | _ :: xs -> find_FLG xs
    in
    match find_FLG xs with
    | [] -> None
    | [x] -> Some x
    | _ -> assert false (* multi FLG *)
  in
  (xs, 
   match flg with None -> [] | Some s -> get_opened_modules_in_FLG s)

let load_path_of_dir dir =
  let dot_merlin = Filename.concat dir ".merlin" in
  if Sys.file_exists dot_merlin then begin
    let lines, opened_modules = load dot_merlin in
    Some (
      List.filter_map (function
           | B x -> 
               if Filename.is_relative x then Some (Filename.concat dir x)
               else Some x
           | _ -> None) lines,
      opened_modules)
  end else None

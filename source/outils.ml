open Root

(* ===== REMOVE ===== *)
let rec remove path = 
  if Sys.file_exists path
  then match Sys.is_directory path with
  | true ->
    Sys.readdir path |>
    Array.iter (fun name -> remove (Filename.concat path name));
    Unix.rmdir path
  | false ->
    Sys.remove path
(* ================== *)


(* ===== FIND THE REPO ===== *)
let repo_find_with_path path =
  if not (Sys.file_exists path)
  then raise Not_exists ;
  let rec aux p =
    let mg = Filename.concat p ".mongit" in
    if Sys.file_exists mg then mg
    else 
      let parent = Filename.dirname p in
      if p = parent then raise No_repo
      else aux parent
  in
  aux (Unix.realpath path)

let repo_find () = repo_find_with_path "."
(* ================== *)
  

(* ===== FILENAME CONCAT LIST ===== *)
let fn_concat_list l =
  List.fold_left Filename.concat (List.hd l) (List.tl l)
(* ================== *)
  

(* ===== HASH ===== *)
let mksha f =
  let ic = open_in f in
  let str_h = Sha1.(to_hex (channel ic (-1))) in
  close_in ic ;
  str_h

let cut_sha str_h =
  let key = String.sub str_h 0 2 in
  let rest = String.sub str_h 2 (String.length str_h - 2) in
  key,rest
(* ================== *)
  

(* ===== COMPRESS / UNCOMPRESS ===== *)
let compress infile outfile =
  let ic = open_in_bin infile
  and oc = open_out_bin outfile in
  Zlib.compress (fun buf -> input ic buf 0 (Bytes.length buf))
                (fun buf len -> output oc buf 0 len);
  close_in ic;
  close_out oc

let uncompress_opened ic oc =
  Zlib.uncompress (fun buf -> input ic buf 0 (Bytes.length buf))
                  (fun buf len -> output oc buf 0 len)

let uncompress infile outfile =
  let ic = open_in_bin infile
  and oc = open_out_bin outfile in
  uncompress_opened ic oc;
  close_in ic;
  close_out oc

(* ================== *)


(* ===== STORE / LOAD ===== *)
let store = function
  | File f ->
    let repo = repo_find_with_path f in
    let key,rest = mksha f |> cut_sha in
    let dir = Filename.concat repo ("files/"^key) in
    if not (Sys.file_exists dir)
    then Sys.mkdir dir Root.mkdir_num ;
    let out = Filename.concat dir rest in
    compress f out
(* ================== *)



    










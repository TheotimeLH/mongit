open Root
open Printf

(* === Small fonctions with not dependancies=== *)
let fn_concat_list l =
  List.fold_left Filename.concat (List.hd l) (List.tl l)

let empty_file f =
  let oc = open_out_gen [Open_creat] mkfile_num f in
  close_out oc

let rec list_rm_fst_occ x = function
  | [] -> []
  | h::q -> if x=h then q else h::list_rm_fst_occ x q

let readlines f =
  let ic = open_in f in
  let l = ref [] in
  begin try while true do
    l := (input_line ic) :: !l
  done with | End_of_file -> () end ;
  close_in ic ;
  Array.of_list (List.rev !l)

(* ================== *)


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
  

(* ===== CHECK ===== *)
let repo_find_chk () =
  try repo_find ()
  with | No_repo ->
    eprintf "Error : no repo found (cwd : \"%s\")\n" (Unix.getcwd ()) ;
    exit 1
    
let exists_chk f =
  if not (Sys.file_exists f) then
  ( eprintf "Error : the path \"%s\" did not match any file\n" f ;
    exit 1 )
(* ================== *)


(* ===== HASH ===== *)
let sha_name nm =
  Sha1.(to_hex (string nm))

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


(* ===== STORE / LOAD / RM ===== *)
let store f dir =
  let key,rest = mksha f |> cut_sha in
  let subdir = Filename.concat dir key in
  if not (Sys.file_exists subdir)
    then Sys.mkdir subdir Root.mkdir_num ;
  let out = Filename.concat subdir rest in
  compress f out

let load str_h dir oc =
  let key,rest = cut_sha str_h in
  let file = fn_concat_list [dir;key;rest] in
  exists_chk file ;
  let ic = open_in_bin file in
  uncompress_opened ic oc

let remove_hash dir str_h =
  let key,rest = cut_sha str_h in
  let subdir = Filename.concat dir key in
  let file = Filename.concat subdir rest in
  Sys.remove file ;
  try Sys.rmdir subdir 
  with | _ (* not empty *) -> ()
(* ================== *)




open Root
open Printf

(* === Small fonctions with not dependancies=== *)
let fn_concat_list l =
  List.fold_left Filename.concat (List.hd l) (List.tl l)

let empty_file f =
  let oc = open_out f in
  close_out oc

let init_file f s =
  print_debug "try to init : %s with %s \n" f s ;
  let oc = open_out f in
  output_string oc s ;
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

let str_list l =
  List.map string_of_int l
  |> (String.concat " ")

let np s = if s="." then "" else s

(* complètement naif, mais marche pour 'a list *)
let rec list_uniq = function 
  | [] -> []
  | h :: q -> if List.mem h q then list_uniq q else h :: list_uniq q

let append rl x =
  rl := x :: !rl
let extend rl l =
  rl := l @ !rl
(* ================== *)


(* ===== SAFE REAL PATH ===== *)
let safe_realpath path =
  let cwd = Unix.getcwd () in
  let s = Filename.concat cwd path in
  let b = Buffer.create 1024 in
  let l_path = ref [] in
  let fct () =
    let sb = Buffer.contents b in
    Buffer.reset b ;
    if sb="." || sb="" then ()
    else if sb=".." then 
      begin l_path := match !l_path with [] -> [] | _::q -> q end
    else l_path := sb :: !l_path
  in

  for i = 0 to String.length s - 1 do
    if s.[i]='/' then fct ()
    else Buffer.add_char b s.[i]
  done ;
  fct () ;
  List.fold_right (fun f d -> Filename.concat d f) !l_path "/"
(* ================== *)

(* ===== REMOVE / CREATE ===== *)
let rec remove path = 
  if Sys.file_exists path
  then match Sys.is_directory path with
  | true ->
    Sys.readdir path |>
    Array.iter (fun name -> remove (Filename.concat path name));
    Unix.rmdir path
  | false ->
    Sys.remove path

let rec create_dir path =
  if not (Sys.file_exists path)
  then (
    create_dir (Filename.dirname path) ;
    Unix.mkdir path Root.mkdir_num
  )
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
  aux (safe_realpath path)

let repo_find () = repo_find_with_path "."
(* ================== *)
  

(* ===== CHECK ===== *)
let repo_find_chk () =
  try repo_find ()
  with | No_repo ->
    eprintf "Error : no repo found (cwd : \"%s\")\n" (Unix.getcwd ()) ;
    raise Mg_error
    
let exists_chk f =
  if not (Sys.file_exists f) then
  ( eprintf "Error : the path \"%s\" did not match any file\n" f ;
    raise Mg_error )
(* ================== *)


(* ==== ROOT PATH ==== *)
let rootpath f =
  let full = safe_realpath f in
  if not (String.starts_with ~prefix:!root full) then
  ( eprintf "Error : the path \"%s\" leads out of the repo\n" f ;
    exit 1)
 else if full = !root then ""
  else (
    let lf = String.length full in
    let lr = String.length !root in
    String.sub full (lr+1) (lf-lr-1)
  )
  
let root_to_realpath f =
  Filename.concat !Root.root f
(* ================== *)

(* ===== BRANCH ===== *)
let find_branch () =
  let f = Filename.concat !repo "branches/HEAD" in
  let ic = open_in f in
  branch := input_line ic ;
  close_in ic

let with_branch s =
  sprintf "%s:%s" !branch s

let find_commit () =
  let f = Filename.concat !dr_brnch !branch in
  let ic = open_in f in
  Scanf.sscanf (input_line ic) "last commit : %s" 
  (fun s -> close_in ic ; s)
(* ================== *)


(* ===== INIT ===== *)
let init () =
  Root.repo := repo_find_chk () ;
  Root.root     := Filename.dirname !repo ;
  Root.dr_trees := Filename.concat !repo "trees" ;
  Root.dr_comms := Filename.concat !repo "commits" ;
  Root.dr_files := Filename.concat !repo "files" ;
  Root.dr_brnch := Filename.concat !repo "branches" ;
  Root.to_be    := Filename.concat !repo "to_be_commited" ;
  find_branch ()
(* ================== *)


(* ===== HASH ===== *)
let sha_name nm =
  Sha1.(to_hex (string nm))

let mksha f =
  exists_chk f ;
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


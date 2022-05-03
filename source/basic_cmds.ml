open Root
open Printf

(* ===== INIT ===== *)
let init () =
  print_debug "Execute init : \n" ;
  if Sys.file_exists "./.mongit" then 
   (eprintf "Error : there is already a repo right here.\n" ;
    exit 1) ;

  List.iter (fun name -> Unix.mkdir name Root.mkdir_num)
    ["./.mongit" ;
     "./.mongit/files" ;
     "./.mongit/commits"]
(* ================ *)


(* ===== REMOVE ===== *)
let remove () =
  print_debug "Try to remove the repo : \n" ;
  if not (Sys.file_exists "./.mongit") then
   (eprintf "Error : there is no repo right here.\n\
      To delete a repo you must be in the root file of your project,\n\
      i.e. the parent file of the \".mongit\" directory.\n" ;
    exit 1) ;

  printf "Do you really want to delete the repo ? [yes or no]\n" ;
  if read_line () = "yes" then Outils.remove "./.mongit"
(* ================ *)
  

(* ===== HASH-FILE ===== *)
let hash_file f =
  try Outils.store (File f)
  with
  | No_repo ->
      eprintf "Error : \"%s\" isn't related to any repo\n" f ;
      exit 1
  | Not_exists ->
      eprintf "Error : the path \"%s\" did not match any file\n" f ;
      exit 1
(* ================ *)


(* ===== CAT-FILE ===== *) 
let cat_file str_h =
  try
    let repo = Outils.repo_find () in
    let key,rest = Outils.cut_sha str_h in
    let file = Outils.fn_concat_list [repo;"files";key;rest] in
    if not (Sys.file_exists file) then raise Not_exists ;
    let ic = open_in_bin file in
    Outils.uncompress_opened ic stdout
  with
  | No_repo ->
      eprintf "Error : no repo found (cwd : \"%s\")\n" (Unix.getcwd ()) ;
      exit 1
  | Not_exists ->
      eprintf "Error : the sha key \"%s\" isn't used\n" str_h ;
      exit 1
(* ================ *)

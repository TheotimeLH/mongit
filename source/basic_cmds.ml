open Root
open Printf

(* ===== INIT ===== *)
let init () =
  print_debug "Execute init : \n" ;
  if Sys.file_exists ".mongit" then 
   (eprintf "Error : there is already a repo right here.\n" ;
    exit 1) ;

  List.iter (fun name -> Unix.mkdir name Root.mkdir_num)
    [".mongit" ;
     ".mongit/files" ;
     ".mongit/commits" ;
     ".mongit/trees" ] ;

  Outils.empty_file 
    (Unix.realpath "." 
    |> Outils.sha_name 
    |> (Filename.concat ".mongit/trees")) ;
  Outils.empty_file ".mongit/trees/files"
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
  if read_line () = "yes" then Outils.remove ".mongit"
(* ================ *)
  

(* ===== HASH-FILE ===== *)
let hash_file f =
  let repo = Outils.repo_find_chk () in
  Outils.exists_chk f ;
  Outils.store f (Filename.concat repo "files")
(* ================ *)


(* ===== CAT ===== *) 
let cat_file str_h =
  let dir = Filename.concat (Outils.repo_find_chk ()) "files" in
  Outils.load str_h dir stdout

let cat_commit str_h =
  let dir = Filename.concat (Outils.repo_find_chk ()) "commits" in
  Outils.load str_h dir stdout
(* ================ *)

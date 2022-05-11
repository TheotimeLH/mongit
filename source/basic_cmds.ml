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
     ".mongit/trees" ;
     ".mongit/branches"] ;

  Outils.init_file ".mongit/branches/HEAD" "Initial\n" ;
  Outils.init_file ".mongit/branches/Initial" "last commit : none\n" ;
  Outils.empty_file ".mongit/to_be_commited" ;
  Outils.empty_file 
    ("Initial:" (* rootpath of root + on the initial branch*)
    |> Outils.sha_name 
    |> (Filename.concat ".mongit/trees"))
(* ================ *)


(* ===== REMOVE ===== *)
let remove_repo () =
  print_debug "Try to remove the repo : \n" ;
  if not (Sys.file_exists "./.mongit") then
   (eprintf "Error : there is no repo right here.\n\
      To delete a repo you must be in the root file of your project,\n\
      i.e. the parent file of the \".mongit\" directory.\n" ;
    exit 1) ;

  printf "Do you really want to delete the repo ? [yes or no]\n" ;
  if read_line () = "yes" then Outils.remove ".mongit"
(* ================ *)
  

(* ===== UPDATE ===== *)
let update () =
  Root.real_cwd := Unix.getcwd () ;
  Unix.chdir "/home/theotime/Documents/Projets/mongit/source" ;
  let ret = Sys.command "make cmd" in
  if ret <> 0 then
    ( eprintf "Mongit update crashed.\n" ;
      exit 1) ;
  Unix.chdir !Root.real_cwd
(* ================ *)


(* ===== HASH-FILE ===== *)
let hash_file f =
  Outils.init () ;
  Outils.exists_chk f ;
  Outils.store f !dr_files
(* ================ *)


(* ===== CAT ===== *) 
let cat_file str_h =
  Outils.init () ;
  Outils.load str_h !dr_files stdout

let cat_commit str_h =
  Outils.init () ;
  Outils.load str_h !dr_comms stdout
(* ================ *)

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
  Outils.empty_file ".mongit/files/all_fkeys" ;
  Outils.empty_file 
    ("Initial:" (* rootpath of root + on the initial branch*)
    |> Outils.sha_name 
    |> (Filename.concat ".mongit/trees")) ;

  printf "A new repo has been created.\n"
(* ================ *)


(* ===== REMOVE ===== *)
let remove_repo () =
  print_debug "Try to remove the repo : \n" ;
  if not (Sys.file_exists "./.mongit") then
   (eprintf "Error : there is no repo right here.\n\
      To delete a repo you must be in the root file of your project,\n\
      i.e. the parent file of the \".mongit\" directory.\n" ;
    exit 1) ;

  printf 
    "Do you really want to delete the repo ? [\"yes\" or no]\n\
     /!\\ it is an unrecoverable destructive action /!\\\n" ;
  if read_line () = "yes" 
  then 
  ( Outils.remove ".mongit" ; 
    printf "the entire repo has been deleted.\n" )
  else printf "Action cancelled.\n"
(* ================ *)
  

(* ===== UPDATE ===== *)
let update () =
  Root.real_cwd := Unix.getcwd () ;
  Unix.chdir "/home/theotime/Documents/Projets/mongit/source" ;
  let ret = Sys.command "make cmd" in
  if ret <> 0 then
    ( eprintf "Mg update crashed.\n" ;
      exit 1)
  else printf "Mg has been successfully updated !\n" ;
  Unix.chdir !Root.real_cwd
(* ================ *)


(* ===== CAT ===== *) 
let cat_file sha =
  Outils.init () ;
  printf "=== File associated with sha %s in the repo : ===\n" sha;
  Outils.load sha !dr_files stdout

let cat_commit sha =
  Outils.init () ;
  printf "=== Commit associated with sha %s in the repo : ===\n" sha;
  Outils.load sha !dr_comms stdout

let cmd_list_commits () =
  Outils.init () ;
  let commits = Outils.list_sha !dr_comms in
  printf "Existing commits :\n%s\n" (String.concat "\n" commits)

let cmd_list_files () =
  Outils.init () ;
  let files = Outils.list_sha !dr_files in
  printf "Files/Versions stored :\n%s\n" (String.concat "\n" files)
(* ================ *)

(* ===== RESET COMMIT ===== *)
let cmd_reset_commit () =
  Outils.init () ;
  Outils.empty_file !to_be ;
  printf "The file to_be_commited has been cleared.\n"
(* ================ *)

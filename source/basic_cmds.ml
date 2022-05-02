open Root
open Printf

(* ===== INIT ===== *)
let init () =
  print_debug "Execute init : \n" ;
  if Sys.file_exists "./.mongit" then 
   (eprintf "Error : there is already a repo right here.\n" ;
    exit 1) ;

  Unix.mkdir "./.mongit" 509
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
  


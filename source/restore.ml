open Root
open Printf

(* ===== RESTORE ===== *)
let cmd_restore df = (* dir or file *) 
  Outils.init () ;
  let rpath = Outils.rootpath df in
  Root.real_cwd := Unix.getcwd () ;
  Unix.chdir !root ;
  (* On liste d'abord tous les fichiers à changer. *)
  let l_files = 
    try Tree.enumerate_d_or_f !dr_trees rpath
    with | Not_in_the_tree ->
      eprintf "\"%s\" hasn't been found in the actual branch.\n" df ;
      exit 1 
  in
  let l_to_restore = 
    List.filter 
      (fun (f,key) -> not (Sys.file_exists f) || (Outils.mksha f <> key))
      l_files
  in
  if l_to_restore = []
  then printf "Nothing to restore, the version stored match the real one.\n"
  else begin
    printf 
      "Do you really want to restore the following %d file(s) ? \
       (yes or no)\n\t%s\n"
      (List.length l_to_restore) 
      (String.concat " " (fst (List.split l_to_restore))) ; 
    if read_line () = "yes" then begin
      let fct (fn,key) =
        Outils.create_dir (Filename.dirname fn) ;
        let oc = open_out fn in
        Outils.load key dr_files oc ;
        close_out oc
      in
      List.iter fct l_to_restore ;
      printf "Done.\n"
    end
  end ;
  Unix.chdir !Root.real_cwd 
(* ================ *)


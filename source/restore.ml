open Root
open Printf

(* ===== RESTORE ===== *)
let cmd_restore df = (* dir or file *) 
  Outils.init () ;
  let rpath = Outils.rootpath df in
  Root.real_cwd := Unix.getcwd () ;
  Unix.chdir !root ;
  (* On liste d'abord tous les fichiers à changer. *)
  let l_dir,l_files = 
    try Tree.enumerate_unk rpath
    with | Not_in_the_tree ->
      eprintf "\"%s\" hasn't been found in the actual branch.\n" df ;
      exit 1 
  in
  let l_f_to_restore = 
    List.filter 
      (fun (f,key) -> not (Sys.file_exists f) || (Outils.mksha f <> key))
      l_files
  in
  let l_d_to_restore = List.filter (fun d -> not (Sys.file_exists d)) l_dir in
  let nb_f = List.length l_f_to_restore
  and nb_d = List.length l_d_to_restore in
  if nb_f=0 && nb_d=0
  then printf "Nothing to restore, the version stored match the real one.\n"
  else begin
    if nb_d<>0 then begin
      List.iter Outils.create_dir l_d_to_restore ;
      printf "%d dir(s) have been restored.\n" nb_d
    end ;
    if nb_f<>0 then begin
      printf 
        "Do you really want to restore the following %d file(s) ? \
         (yes or no)\n\t%s\n"
        nb_f (String.concat " " (fst (List.split l_f_to_restore))) ; 
      if read_line () = "yes" then begin
        let fct (fn,key) =
          Outils.create_dir (Filename.dirname fn) ;
          let oc = open_out fn in
          Outils.load key !dr_files oc ;
          close_out oc
        in
        List.iter fct l_f_to_restore ;
        printf "Done.\n"
      end
    end ;
  end ;
  Unix.chdir !Root.real_cwd 
(* ================ *)


open Root
open Printf

(* ===== cmd_add ===== *)
(* La commande add ajoute juste la liste des fichiers à ajouter. 
   Sachant qu'on peut git add un dossier et il y aura juste son nom. *)
let cmd_add b f =
  try
    let repo = Outils.repo_find () in
    if not (Sys.file_exists f) then raise Not_exists ;
    let oc = open_out_gen [Open_creat ; Open_append] mkfile_num
      (Filename.concat repo "to_be_commited.txt") in
    output_string oc (sprintf "%s %s\n" (if b then "add" else "minus") f) ;
    close_out oc
  with
  | No_repo ->
      eprintf "Error : no repo found (cwd : \"%s\")\n" (Unix.getcwd ()) ;
      exit 1
  | Not_exists ->
      eprintf "Error : the path \"%s\" did not match any file\n" f ;
      exit 1
(* ================ *)

(* let cmd_commit *)

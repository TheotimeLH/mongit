open Root
open Printf

let msg = ref "" 
(* ===== cmd_add ===== *)
(* La commande add ajoute juste la liste des fichiers à ajouter. 
   Sachant qu'on peut git add un dossier et il y aura juste son nom. *)
let cmd_add b f =
  let repo = Outils.repo_find_chk () in
  Outils.exists_chk f ;
  let oc = open_out_gen [Open_creat ; Open_append] mkfile_num
    (Filename.concat repo "to_be_commited") in
  output_string oc (sprintf "%s %s\n" (if b then "add" else "minus") f) ;
  close_out oc
(* ================ *)

let mk_todo_list ic =
  let tl = ref [] in




(* ===== cmd_commit ===== *)
let cmd_commit () =
  let repo = Outils.repo_find_chk () in
  let f = Filename.concat repo "to_be_commited" in
  if not (Sys.file_exists f) then
    printf "Nothing to commit, use mg -add first.\n"
  else begin
    
  end
(* ================ *)

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

type addtype = 
  | New of string (* filename *)
  | Change of string * string (* old sha key, filename *)

let mk_todo_list ic =
  let tl = ref [] in
  (* Utiliser .mongit/trees/files qui a des lignes filename -> sha key actuel
     les mettre dans une table pour savoir qui est déjà présent
     pour ajouter un file, on regarde si présent via la table
     si besoin on l'add en Unix.realpath.
     Pour un dir, on regarde si déjà présent en regardant si
     le fichier associé existe. Dans tous les cas on partira récursivement
     sur les fils mais parce que dans la liste après
     etape 1, 
     Préprocess les add et minus avant, en ouvrant les dirs
     et faire une liste des add à faire dans l'ordre
     etape 2, faire ce qui précède, et donc savoir addtype
     et faire l'arbre.
     etape 3, avec la liste des files à add (osef l'ordre) en addtype, 
     on les traite 1 par 1, en utilisant le scan_diff
*)





(* ===== cmd_commit ===== *)
let cmd_commit () =
  let repo = Outils.repo_find_chk () in
  let f = Filename.concat repo "to_be_commited" in
  if not (Sys.file_exists f) then
    printf "Nothing to commit, use mg -add first.\n"
  else begin
    
  end
(* ================ *)

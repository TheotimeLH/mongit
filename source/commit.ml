open Root
open Printf
(* Pour commit :
  On commence par énumerer les fichiers, et voir lesquels sont des changements
  et lesquels sont créés. Puis on les traite effectivement. Et ceux qui n'ont 
  pas crash (faire un try with _ ->) sur la création, ils vont être add au tree.
  Et une fois fini, on update le tree. *)
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

let mk_todo_list repo =
  let to_be   = Filename.concat repo "to_be_commited" in
  let files   = Filename.concat repo "trees/files" in
  let dr_tree = Filename.concat repo "trees" in
  (* Etape 1 : tbl filename -> sha key actuel (si existe) *)
  let hfiles = Hashtbl.create 10 in
  let ic = Scanf.Scanning.open_in files in
  try while true do
    Scanf.bscanf ic "%s %s\n" (Hashtbl.add hfiles)
  done with | End_of_file -> () ;
  Scanf.Scanning.close_in ic ;
  (* === *)

  (* Les fonctions auxiliaires principales *)
  let tl = ref [] in
  let dir_to_crt = ref [] in
  (* ADD *)
  let add_f f = (* en realpath *)
    match Hashtbl.find_opt hfiles f with
    | None -> tl := New f :: !tl
    | Some key -> tl := Change(key,f) :: !tl
  in
  let add_d d =
    let ds = Filename.concat dr_tree (Outils.sha_name d) in
    if not (Sys.file_exists ds) then dir_to_crt := d :: !dir_to_crt ;
    Array.iter ((fun df -> add_d_or_f (Filename.concat d df)) (Sys.readdir d)
  and add_d_or_f df =
    if Sys.is_directory df then add_d df else add_f df
  in

  (* MINUS *)
  let minus_f f =
    let rec suppr = function
      | [] -> []
      | New x::q | Change(_,x)::q when x=f -> q
      | h::q -> h :: suppr q 
    in
    tl := suppr !tl
  in
  let minus_d d =
    dir_to_crt := Outils.list_rm_fst_occ d !dir_to_crt ;
    Array.iter ((fun df -> minus_d_or_f (Filename.concat d df)) (Sys.readdir d)
  and minus_d_or_f df =
    if Sys.is_directory df then minus_d df else minus_f df
  in
  (* === *)

  let ic = Scanf.Scanning.open_in to_be in
  try while true do
    Scanf.bscanf ic "%s %s\n"
    (fun a df -> Outils.exists_chk df ;
      let df = Unix.realpath df in
      if a = "add" then add_d_or_f df else minus_d_or_f df)
  done with | End_of_file -> () ;
  Scanf.Scanning.close_in ic ;

  !tl , !dir_to_crt




(* ===== cmd_commit ===== *)
let cmd_commit () =
  let repo = Outils.repo_find_chk () in
  let f = Filename.concat repo "to_be_commited" in
  if not (Sys.file_exists f) then
    printf "Nothing to commit, use mg -add first.\n"
  else begin
    
  end
(* ================ *)

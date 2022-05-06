open Root
open Scan_diff
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
(* === Ref === *)
let msg = ref ""
let nb_new = ref 0
let nb_rebuilt = ref 0
let nb_minor = ref 0
let to_suppr = ref []

let tbl_files = ref IdMap.empty 
let load_tbl_files repo =
  let ic = Scanf.Scanning.open_in (Filename.concat repo "trees/files") in
  try while true do
    Scanf.bscanf ic "%s %s\n" (*(Hashtbl.add tbl_files)*)
    (fun f key -> tbl_files := IdMap.add f key !tbl_files)
  done with | End_of_file -> () ;
  Scanf.Scanning.close_in ic 
  

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

(* ===== ETAPE 1 ===== *)
let mk_todo_list repo =
  let to_be    = Filename.concat repo "to_be_commited" in
  let dr_trees = Filename.concat repo "trees" in
  load_tbl_files repo ;

  (* Les fonctions auxiliaires principales *)
  let tl = ref [] in
  let dir_to_crt = ref [] in
  (* ADD *)
  let add_f f = (* en realpath *)
    match IdMap.find_opt f !tbl_files with
    | None -> tl := New f :: !tl
    | Some key -> tl := Change(key,f) :: !tl
  in
  let add_d d =
    let ds = Filename.concat dr_trees (Outils.sha_name d) in
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
(* ================ *)

(* ===== add_real / effectif ===== *)
let add_real commit_ch dr_files t =
  begin match t with
  | New f ->
      incr nb_new ;
      Outils.store f dr_files ;
      let key = Outils.mksha f in
      fprintf commit_ch "NEW %s ; key = %s\n\n" f key
  | Change (old_key,f) ->
    (* Store la nouvelle version *)
      Outils.store f dr_files ;
      let new_key = Outils.mksha f in
      Hashtbl.add tbl_files f new_key ;

    (* Charger l'ancienne, via un fichier où la décompresser *)
      let tmp_old_file = Filename.concat dr_files "tmp_old_file" in
      let old_ch = open_out_gen [Open_creat] mkfile_num tmp_old_file in
      Outils.load old_key dr_files old_ch ;
      close_out old_ch ;
    (* Utiliser Scan_diff pour calculer les différences *)
      let old_ch = open_in tmp_old_file
      and new_ch = open_in f in
      let nb_diff,old_in_new = Scan_diff.main old_ch new_ch in
      close_in old_ch ; close_in new_ch ;
    (* Charger les deux versions *)
      let t_old = Outils.readlines tmp_old_file in
      let t_new = Outils.readlines f in
      let lenO = Array.length t_old in
      let lenN = Array.length t_new in
    (* Voir si on garde l'ancien *)
      if (nb_diff * 2 > lenN) && (lenO > 5)
      then begin 
      (* REBUILT : ON GARDE en mémoire l'old et le new *)
        incr nb_rebuilt ;
        fprintf commit_ch "REBUILT %s ; old_key = %s ; new_key = %s\n\n"
          f old_key new_key ;
      end
      else begin
      (* MINOR : On suppr old, et on note dans le fichier de commit les différences *)
      (* On doit noter ce qui appartenait à old. 
         Ce qui appartient à new on l'a toujours *)
        incr nb_minor ;
        fprintf commit_ch "MINOR %s ; new_key = %s, %d changes :\n"
          f new_key (List.length old_in_new) ;
        to_suppr := old_key :: !to_suppr ;
        let aux_modif = function (* : Scan_diff.change type *)
          | Add(i,j) -> 
              fprintf commit_ch "Add l-%d to l-%d (new numbering)\n" i j
          | Remove(i,j) ->
              fprintf commit_ch "Remove l-%d to l-%d (old numbering) :\n" i j ;
              for l = i to j do
                input_string commit_ch t_old.(l)
              done
          | Modif ((i_old,j_old),(i_new,j_new)) ->
              fprintf commit_ch 
                "Modif from l-%d,l-%d (old numbering) \
                 to l-%d,l-%d (new), rm :\n" i_old j_old i_new j_new ;
              for l = i_old to j_old do
                input_string commit_ch t_old.(l)
              done
        in
        List.iter aux_modif old_in_new
      end ;
      Sys.remove tmp_old_file
      
  end ;
  printf "*" ; flush stdout
(* ================ *)


(* ===== cmd_commit ===== *)
let cmd_commit () =
  let repo = Outils.repo_find_chk () in
  let to_be    = Filename.concat repo "to_be_commited" in
  let files    = Filename.concat repo "trees/files" in
  let dr_comms = Filename.concat repo "commits" in
  let dr_trees = Filename.concat repo "trees" in
  let dr_files = Filename.concat repo "files" in
  if not (Sys.file_exists to_be) then
    eprintf "Nothing to commit, use mg -add first.\n"
  else begin
    printf "Listing..." ; flush stdout ;
    let todo_list , dir_to_crt = mk_todo_list repo in
    printf "%d files to handle\n" (List.length todo_list) ; flush stdout ;
    let tmp_file = Filename.concat dr_comms "tmp_commit_file" in
    let commit_ch = open_out_gen [Open_creat ; Open_trunc] mkfile_num tmp_file in
    List.iter (add_real commit_ch dr_files) todo_list ;
    fprintf commit_ch "\n\nMESSAGE :\n%s" !msg ;
    close_in commit_ch ;
    Outils.store tmp_file dr_comms ;
    if not !bool_print_debug then Sys.remove tmp_file ;
    (* Maintenant que les nouvelles versions sont save, je 
       mets à jour la liste des fichiers : trees/files et je
       suppr les anciens. Je préfère supprimer les fichiers à la
       fin. Comme ça, si bug il y a, on n'a rien perdu. *)
    let oc = open_out_gen [Open_trunc] 0 files in
    IdMap.iter (fprintf oc "%s %s\n") !tbl_files ;
    close_out oc ;
    List.iter Outils.remove_hash !to_suppr
  end
(* ================ *)

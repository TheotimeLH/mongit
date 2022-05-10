open Root
open Scan_diff
open Printf

(* === Ref === *)
let msg = ref ""
let nb_new = ref 0
let nb_rebuilt = ref 0
let nb_minor = ref 0
let nb_nothing = ref 0
let to_suppr = ref []
let nb_lines_add = ref 0
let nb_lines_del = ref 0
let tbl_files = ref IdMap.empty 

(* ===== ETAPE 1 ===== *)
let mk_todo_list repo =
  let to_be    = Filename.concat repo "to_be_commited" in
  let dr_trees = Filename.concat repo "trees" in
  tbl_files := Outils.load_tbl_files repo ;

  (* Les fonctions auxiliaires principales *)
  let tl = ref [] in
  let dir_to_crt = ref [] in
  (* ADD *)
  let add_f f = (* en rootpath MAIS cwd = root*)
    match IdMap.find_opt f !tbl_files with
    | None -> tl := New f :: !tl
    | Some key -> tl := Change(key,f) :: !tl
  in
  let rec add_d d = (* en rootpath *)
    let ds = Filename.concat dr_trees (Outils.sha_name d) in
    if not (Sys.file_exists ds) then dir_to_crt := d :: !dir_to_crt ;
    Array.iter 
      (fun df ->
        if !include_secret || df.[0] <> '.' 
        then add_d_or_f (Filename.concat d df)) 
      (Sys.readdir d)
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
  let rec minus_d d = (* en rootpath et root=cwd *)
    dir_to_crt := Outils.list_rm_fst_occ d !dir_to_crt ;
    Array.iter 
      (fun df -> 
        if !include_secret || df.[0] <> '.' 
        then minus_d_or_f (Filename.concat d df)) 
      (Sys.readdir d)
  and minus_d_or_f df =
    if Sys.is_directory df then minus_d df else minus_f df
  in
  (* === *)

  let ic = Scanf.Scanning.open_in to_be in
  begin try while true do
    Scanf.bscanf ic "%s %s\n"
    (fun a df -> 
      if a="all" then begin
        Array.iter 
          (fun sub -> if !include_secret || sub.[0] <> '.' then 
           if df = "add" then add_d_or_f sub else minus_d_or_f sub) 
          (Sys.readdir ".")
      end
      else begin
      Outils.exists_chk df ;
      if a="add" then add_d_or_f df else minus_d_or_f df
      end)
  done with | End_of_file -> () end ;
  Scanf.Scanning.close_in ic ;

  print_debug "dir_to_crt : %s\n" (String.concat " " !dir_to_crt) ;
  List.iter (Tree.add_dir dr_trees) !dir_to_crt ;
  !tl
(* ================ *)

(* ===== add_real / effectif ===== *)
let add_real commit_ch dr_files dr_trees t =
  begin match t with
  | New f -> (* en rootpath et root=cwd*)
      incr nb_new ;
      Outils.store f dr_files ;
      let key = Outils.mksha f in
      Tree.add_file dr_trees f key;
      tbl_files := IdMap.add f key !tbl_files ;
      fprintf commit_ch "NEW %s ; key = %s\n\n" f key
  | Change (old_key,f) ->
      let new_key = Outils.mksha f in
      if old_key <> new_key then begin
    (* Store la nouvelle version *)
      Outils.store f dr_files ;
      tbl_files := IdMap.add f new_key !tbl_files ;
    (* Charger l'ancienne, via un fichier où la décompresser *)
      let tmp_old_file = Filename.concat dr_files "tmp_old_file" in
      let old_ch = open_out tmp_old_file in
      Outils.load old_key dr_files old_ch ;
      close_out old_ch ;
    (* Utiliser Scan_diff pour calculer les différences *)
      let old_ch = open_in tmp_old_file
      and new_ch = open_in f in
      let nb_ok,old_in_new,t_placeN = Scan_diff.main old_ch new_ch in
      close_in old_ch ; close_in new_ch ;
      print_debug "Modif de %s, nb = %d, distribution :\n%s\n" f nb_ok
        (Outils.str_list (Array.to_list t_placeN)) ;
      let nb_del,nb_add = Scan_diff.count_diff old_in_new in
      nb_lines_del := nb_del + !nb_lines_del ;
      nb_lines_add := nb_add + !nb_lines_add ;
    (* Charger les deux versions *)
      let t_old = Outils.readlines tmp_old_file in
      let lenO = Array.length t_old in
      let lenN = Array.length t_placeN in
    (* Voir si on garde l'ancien *)
      if (nb_ok * 2 <= lenN) && (lenO > 5)
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
                output_string commit_ch (t_old.(l)^"\n")
              done
          | Modif ((i_old,j_old),(i_new,j_new)) ->
              fprintf commit_ch 
                "Modif from l-%d,l-%d (old numbering) \
                 to l-%d,l-%d (new), rm :\n" i_old j_old i_new j_new ;
              for l = i_old to j_old do
                output_string commit_ch (t_old.(l)^"\n")
              done
        in
        List.iter aux_modif old_in_new
      end ;
      Sys.remove tmp_old_file
      end
      else incr nb_nothing
      
  end ;
  printf "*" ; flush stdout
(* ================ *)


(* ===== cmd_commit ===== *)
let cmd_commit () =
  let repo = Outils.repo_find_chk () in
  let to_be    = Filename.concat repo "to_be_commited" in
  let dr_comms = Filename.concat repo "commits" in
  let dr_trees = Filename.concat repo "trees" in
  let dr_files = Filename.concat repo "files" in
  Root.real_cwd := Unix.getcwd () ;
  Unix.chdir (Filename.dirname repo) ;
  if not (Sys.file_exists to_be) then
    eprintf "Nothing to commit, use mg -add first.\n"
  else begin
    printf "Listing..." ; flush stdout ;
    let todo_list = mk_todo_list repo in
    printf "%d files to handle\n|" (List.length todo_list) ; flush stdout ;
    let tmp_file = Filename.concat dr_comms "tmp_commit_file" in
    (* let commit_ch = open_out_gen [Open_creat ; Open_trunc] mkfile_num tmp_file in *)
    (* Donne un bug trop bizarre *)
    let commit_ch = open_out tmp_file in
    List.iter (add_real commit_ch dr_files dr_trees) todo_list ;
    fprintf commit_ch "\n\nMESSAGE :\n%s" !msg ;
    close_out commit_ch ;
    Outils.store tmp_file dr_comms ;
    if not !bool_print_debug then Sys.remove tmp_file ;
    (* Maintenant que les nouvelles versions sont save, je 
       mets à jour la liste des fichiers : trees/files et je
       suppr les anciens. Je préfère supprimer les fichiers à la
       fin. Comme ça, si bug il y a, on n'a rien perdu. *)
    let oc = open_out files in
    IdMap.iter (fprintf oc "%s %s\n") !tbl_files ;
    close_out oc ;
    List.iter (Outils.remove_hash dr_files) !to_suppr ;
    Sys.remove to_be ;
    printf "\nDone. new : %d ; rebuilt : %d ; minor changes : %d ; no change : %d\n"
      !nb_new !nb_rebuilt !nb_minor !nb_nothing ;
    printf "Total : %d files changed, %d insertions(+), %d deletions(-)\n"
      (!nb_rebuilt + !nb_minor) !nb_lines_del !nb_lines_add ;
  end ;
  Unix.chdir !Root.real_cwd 
(* ================ *)

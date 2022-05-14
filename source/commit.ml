open Root
open Scan_diff
open Printf

(* === Ref === *)
let msg = ref ""
let nb_rebuilt = ref 0
let nb_minor = ref 0
let nb_nothing = ref 0
let nb_lines_add = ref 0
let nb_lines_del = ref 0
let tbl_fkeys = ref IdMap.empty

(* ===== Fonctions effectives, qui add/remove/modifie/mv etc ===== *)

(* == CREATE == *)
let do_d_cr commit_ch d =
  print_detail "Create dir : %s\n" d ;
  Tree.add_d d ;
  fprintf commit_ch "CREATE DIR %s\n" d

let do_f_cr commit_ch f =
  print_detail "Create file : %s\n" f ;
  Outils.store f !dr_files ;
  (*nb_lines_add := !nb_lines_add + (Array.length (Outils.readlines f)) ;*)
  let key = Outils.mksha f in
  Tree.add_f f key ;
  tbl_fkeys := IdMap.add key (IdSet.singleton !branch) !tbl_fkeys ;
  fprintf commit_ch "CREATE FILE %s %s\n" f key ;
  printf "*" ; flush stdout


(* == MOVE == *)
let real_mv (op,np) =
  let ret = Sys.command (sprintf "mv %s %s" op np) in
  if ret<>0 then 
  (eprintf "Sys command mv failed to move %s -> %s" op np ; exit 1)

let begin_d_mv commit_ch (op,np) = (*oldpath newpath*)
  print_detail "Move dir : %s to %s\n" op np ;
  Tree.add_d np ;
  fprintf commit_ch "MOVE DIR %s %s\n" op np
let end_d_mv (op,_) = Tree.remove_d op

let begin_f_mv commit_ch ((op,key),np) =
  print_detail "Move file : %s to %s\n" op np ;
  Tree.add_f np key ;
  fprintf commit_ch "MOVE FILE %s %s %s\n" op np key ;
  printf "*" ; flush stdout
let end_f_mv ((op,_),_) = Tree.remove_f op


(* == REMOVE == *)
let real_rm = Outils.remove

let begin_d_rm commit_ch d =
  print_detail "Remove dir : %s\n" d ;
  fprintf commit_ch "REMOVE DIR %s\n" d
let end_d_rm d = Tree.remove_d d

let begin_f_rm commit_ch (f,key) =
  print_detail "Remove file : %s\n" f ;
  (*nb_lines_del := !nb_lines_del + (Array.length (Outils.readlines f)) ;*)
  fprintf commit_ch "REMOVE FILE %s %s\n" f key ;
  printf "*" ; flush stdout
let end_f_rm (f,_) = Tree.remove_f f


(* == MODIF == *)
(* Compliqué : car on sauvegarde les différences *)

let do_f_ch commit_ch (f,old_key) =
  print_detail "Change file : %s\n" f ;
  let new_key = Outils.mksha f in
  if old_key <> new_key then begin
  (* Store la nouvelle version *)
    Outils.store f !dr_files ;
    Tree.remove_f f ;
    Tree.add_f f new_key ;
    tbl_fkeys := IdMap.add new_key (IdSet.singleton !branch) !tbl_fkeys ;
  (* Charger l'ancienne, via un fichier où la décompresser *)
    let tmp_old_file = Filename.concat !dr_files "tmp_old_file" in
    let old_ch = open_out tmp_old_file in
    Outils.load old_key !dr_files old_ch ;
    close_out old_ch ;
  (* Utiliser Scan_diff pour calculer les différences *)
    let old_ch = open_in tmp_old_file and new_ch = open_in f in
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
      fprintf commit_ch "MODIF REBUILT %s %s %s\n"
        f old_key new_key ;
    end
    else begin
    (* MINOR : On suppr old, et on note dans le fichier de commit les différences *)
    (* On doit noter ce qui appartenait à old. 
       Ce qui appartient à new on l'a toujours *)
      incr nb_minor ;
      fprintf commit_ch "MODIF MINOR %s %s %d\n"
        f new_key (List.length old_in_new) ;
      tbl_fkeys := IdMap.add old_key 
      (IdSet.remove !branch (IdMap.find old_key !tbl_fkeys)) !tbl_fkeys ;
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
  else incr nb_nothing ;
  printf "*" ; flush stdout

let end_f_ch key = Outils.remove_hash !dr_files key
(* ================ *)


(* ===== cmd_commit ===== *)
let cmd_commit () =
  Outils.init () ;
  Outils.rootwd () ;
  tbl_fkeys := Tree.load_tbl_fkeys () ;
  let d_to_cr,f_to_cr,f_to_ch,
    d_to_rm_real,f_to_rm_real,d_to_mv_real,f_to_mv_real,
    d_to_rm_tree,f_to_rm_tree,d_to_mv_tree,f_to_mv_tree,
    _ = Pre_commit.compile_to_be () in

  let nb_d_cr = List.length d_to_cr
  and nb_f_cr = List.length f_to_cr
  and nb_f_ch = List.length f_to_ch
  and nb_f_rm_real = List.length f_to_rm_real
  and nb_f_rm_tree = List.length f_to_rm_tree
  and nb_d_rm_real = List.length d_to_rm_real
  and nb_d_rm_tree = List.length d_to_rm_tree
  and nb_f_mv_real = List.length f_to_mv_real
  and nb_f_mv_tree = List.length f_to_mv_tree
  and nb_d_mv_real = List.length d_to_mv_real
  and nb_d_mv_tree = List.length d_to_mv_tree in
  let nb_f_tree = nb_f_cr + nb_f_ch + nb_f_rm_tree + nb_f_mv_tree
  and nb_d_tree = nb_d_cr + nb_d_rm_tree + nb_d_mv_tree in
  let nb_f_real = nb_f_rm_real + nb_f_mv_real
  and nb_d_real = nb_d_rm_real + nb_d_mv_real in

  if nb_f_tree+nb_d_tree=0 
  then eprintf "Nothing to commit, use mg -add first.\n"
  else begin
    printf "Modification of the branch \"%s\" :\n" !branch ;
    printf "-> %d file(s) to handle (and %d dir(s)) :\n|" nb_f_tree nb_d_tree ;
    flush stdout ;
    (* Idée : On écrit et on enregistre toutes les modifications avant
       de faire quoique ce soit d'irréversible. Donc on enregistre dans
       le commit ce qu'on va suppr, avant de le faire effectivement. *)
    let tmp_file = Filename.concat !dr_comms "tmp_commit_file" in
    let commit_ch = open_out tmp_file in

    let pcommit = Outils.find_commit () in
    (*fprintf commit_ch "\nBranch : %s\n" !branch ;*)
    fprintf commit_ch "Parent commits : %s\n" pcommit ;

    List.iter (do_d_cr commit_ch) d_to_cr ;
    List.iter (do_f_cr commit_ch) f_to_cr ;
    List.iter (do_f_ch commit_ch) f_to_ch ;
    List.iter (begin_d_mv commit_ch) d_to_mv_tree ;
    List.iter (begin_f_mv commit_ch) f_to_mv_tree ;
    List.iter (begin_d_rm commit_ch) d_to_rm_tree ;
    List.iter (begin_f_rm commit_ch) f_to_rm_tree ;

    fprintf commit_ch "\nMessage :\n%s" !msg ;

    close_out commit_ch ;
    Outils.store tmp_file !dr_comms ;
    let oc = open_out (Filename.concat !dr_brnch !branch) in
    fprintf oc "last commit : %s\n" (Outils.mksha tmp_file) ;
    close_out oc ;

    List.iter end_d_mv d_to_mv_tree ;
    List.iter end_f_mv f_to_mv_tree ;
    List.iter end_d_rm d_to_rm_tree ;
    List.iter end_f_rm f_to_rm_tree ;

    let oc = open_out (Filename.concat !dr_files "all_fkeys") in
    IdMap.iter 
    (fun key st -> if st = IdSet.empty then end_f_ch key
      else begin 
        let nb = IdSet.cardinal st in
        fprintf oc "%s %d " key nb ;
        IdSet.iter (fprintf oc "%s ") st ;
        fprintf oc "\n"
    end) !tbl_fkeys ;
    close_out oc ;

    if not !bool_print_debug then Sys.remove tmp_file ;
    printf 
    "\nDone. created : %d ; rebuilt : %d ; slightly modified : %d ; \
      unchanged : %d ; removed : %d ; moved : %d\n"
      nb_f_cr !nb_rebuilt !nb_minor !nb_nothing nb_f_rm_tree nb_f_mv_tree ;
    printf "Total : %d insertions(+), %d deletions(-)\n"
      !nb_lines_add !nb_lines_del ;
  end ;
  
  if nb_f_real+nb_d_real<>0 then begin
    printf 
      "Now we will apply move and remove commands on \
       the real directory (%d file(s) and %d dir(s)). \
       Next time, if you only want to operate on the \
       repo use the -only_on_repo option.\n" nb_f_real nb_d_real ;
    List.iter real_mv d_to_mv_real ;
    List.iter real_mv f_to_mv_real ;
    List.iter real_rm d_to_rm_real ;
    List.iter real_rm f_to_rm_real
  end ;

  Outils.empty_file !to_be ;
  Outils.realwd () 
(* ================ *)


(* let commit_ch = 
    open_out_gen [Open_creat ; Open_trunc] mkfile_num tmp_file in
   Donne un bug trop bizarre *)

open Root
open Scan_diff
open Printf
let len = List.length

(* 
 Le role de commit.ml est d'écrire le fichier de commit 
 en interprétant to_be_commited (lu par le pre_commit). 
 Ensuite on Branch_mvt.forward ce commit si voulu.
*)

(* === Ref === *)
let msg = ref ""
let nb_rebuilt = ref 0
let nb_minor = ref 0
let nb_lines_add = ref 0
let nb_lines_del = ref 0
let tbl_fkeys = ref IdMap.empty

(* ===== Fonctions effectives, qui add/remove/modifie/mv etc ===== *)

(* == CREATE == *)
let do_d_cr cch d =
  print_detail "Create dir : %s\n" d ;
  fprintf cch "CREATE DIR %s\n" d
let do_f_cr_exist cch (f,key) =
  tbl_fkeys := Outils.map_list_add key !branch !tbl_fkeys ;
  print_detail "Create file : %s\n" f ;
  fprintf cch "CREATE FILE %s %s\n" f key ;
  printf "*" ; flush stdout
let do_f_cr cch f =
  Outils.store f !dr_files ;
  let key = Outils.mksha f in
  do_f_cr_exist cch (f,key)


(* == MOVE == *)
let real_mv (op,np) =
  let ret = Sys.command (sprintf "mv %s %s" op np) in
  if ret<>0 then 
  (eprintf "Sys command mv failed to move %s -> %s" op np ; exit 1)
let do_d_mv cch (op,np) = (*oldpath newpath*)
  print_detail "Move dir : %s to %s\n" op np ;
  fprintf cch "MOVE DIR %s %s\n" op np
let do_f_mv cch ((op,key),np) =
  print_detail "Move file : %s to %s\n" op np ;
  fprintf cch "MOVE FILE %s %s %s\n" op np key ;
  printf "*" ; flush stdout


(* == REMOVE == *)
let real_rm = Outils.remove
let do_d_rm cch d =
  print_detail "Remove dir : %s\n" d ;
  fprintf cch "REMOVE DIR %s\n" d
let do_f_rm cch (f,key) =
  print_detail "Remove file : %s\n" f ;
  fprintf cch "REMOVE FILE %s %s\n" f key ;
  printf "*" ; flush stdout


(* == MODIF == *)
(* Compliqué : car on sauvegarde les différences *)
(* Ce sera au Branch_mvt.forward de gérer Tree.add/Outil.store etc *)
(* 2 cas selon si le fichier qu'on veut enregistré est 
   sauvegardé sous forme de clé ou si c'est un vrai fichier. *)

(* fn : filename ; fread : nom du fichier où lire le contenu. *)
let do_f_ch_aux cch fn fread old_key new_key =
  print_detail "Change file : %s\n" fn ;
(* Charger l'ancienne, via un fichier où la décompresser *)
  let tmp_old_file = Filename.concat !dr_files "tmp_old_file" in
  Outils.load_fn old_key !dr_files tmp_old_file ;
(* Utiliser Scan_diff pour calculer les différences *)
  let old_ch = open_in tmp_old_file 
  and new_ch = open_in fread in
  let nb_ok,old_in_new,t_placeN = Scan_diff.main old_ch new_ch in
  close_in old_ch ; 
  close_in new_ch ;
  print_debug "Modif de %s, nb = %d, distribution :\n%s\n" fn nb_ok
    (Outils.str_list (Array.to_list t_placeN)) ;
  let nb_del,nb_add = Scan_diff.count_diff old_in_new in
  nb_lines_del := nb_del + !nb_lines_del ;
  nb_lines_add := nb_add + !nb_lines_add ;
(* Charger les deux versions *)
  let t_old = Outils.readlines tmp_old_file in
  let t_new = Outils.readlines fread in
  let lenO = Array.length t_old in
  let lenN = Array.length t_new in
(* Voir si on garde l'ancien *)
  if (nb_ok * 2 <= lenN) && (lenO > 5)
  then begin 
  (* REBUILT : ON GARDE en mémoire l'old et le new *)
    incr nb_rebuilt ;
    Outils.store fread !dr_files ;
    tbl_fkeys := Outils.map_list_add new_key !branch !tbl_fkeys ;
    fprintf cch "MODIF REBUILT %s %s %s\n" fn old_key new_key ;
  end
  else begin
  (* MINOR : On note dans le fichier de commit les différences *)
    incr nb_minor ;
    fprintf cch "MODIF MINOR %s %s %s %d\n" 
      fn old_key new_key (len old_in_new) ;

    let aux_modif = function (* : Scan_diff.change type *)
      | Add(i,j) -> 
          fprintf cch "Add l-%d to l-%d (new numbering) :\n" i j ;
          for l = i to j do output_string cch (t_new.(l)^"\n") done
      | Remove(i,j) ->
          fprintf cch "Remove l-%d to l-%d (old numbering) :\n" i j ;
          for l = i to j do output_string cch (t_old.(l)^"\n") done
      | Modif ((i_old,j_old),(i_new,j_new)) ->
          fprintf cch 
            "Modif from l-%d,l-%d (old numbering) \
             to l-%d,l-%d (new) :\n" i_old j_old i_new j_new ;
          for l = i_old to j_old do output_string cch (t_old.(l)^"\n") done ;
          fprintf cch "==================================\n" ;
          for l = i_new to j_new do output_string cch (t_new.(l)^"\n") done ;
    in
    List.iter aux_modif old_in_new
  end ;
  Sys.remove tmp_old_file ;
  printf "*" ; flush stdout



let do_f_ch cch (f,old_key) =
  let new_key = Outils.mksha f in
  do_f_ch_aux cch f f old_key new_key

let do_f_ch_exist cch (fn,old_key,new_key) =
  let tmp_new_file = Filename.concat !dr_files "tmp_new_file" in
  Outils.load_fn new_key !dr_files tmp_new_file ;
  do_f_ch_aux cch fn tmp_new_file old_key new_key ;
  Outils.remove tmp_new_file 

(* ================ *)


(* ===== print_commit ===== *)
(* La fonction print_commit sert à la fois pour la cmd_commit,
   avec ce qui suit. Mais aussi pour les commits résultants
   de merge, cf branch_merge.ml
   Donc il faut une fonction générique. *)
let print_commit msg pcommit
      d_to_cr d_to_mv d_to_rm 
      f_to_cr f_to_ch f_to_mv f_to_rm 
      f_to_cr_exist f_to_ch_exist = (* -> le sha du commit *)
  tbl_fkeys := Outils.load_tbl_fkeys () ;
  let tmp_file = Filename.concat !dr_comms "tmp_commit_file" in
  let cch = open_out tmp_file in

  let nb_f = len f_to_cr + len f_to_mv + len f_to_rm + len f_to_ch
  and nb_d = len d_to_cr + len d_to_mv + len d_to_rm in

  fprintf cch "SIMPLE\nParent commit : %s\n" pcommit ;
  fprintf cch "Nb operations : %d\n" (nb_f+nb_d) ;

  (* fn=filename ; k=key ; ok=old_k ; nk=new_k ; op=old_path ; np=new_path *)
  List.iter (do_d_cr cch) d_to_cr ; (* fn *)
  List.iter (do_f_cr cch) f_to_cr ; (* fn *)
  List.iter (do_f_ch cch) f_to_ch ; (* fn*ok *)
  List.iter (do_d_mv cch) d_to_mv ; (* op*np *)
  List.iter (do_f_mv cch) f_to_mv ; (* (op*k)*np *)
  List.iter (do_d_rm cch) d_to_rm ; (* fn *)
  List.iter (do_f_rm cch) f_to_rm ; (* fn*k *) 
  List.iter (do_f_cr_exist cch) f_to_cr_exist ; (* fn*k *)
  List.iter (do_f_ch_exist cch) f_to_ch_exist ; (* fn*ok*nk *)

  fprintf cch "\nMessage :\n%s" msg ;

  close_out cch ;
  let sha = Outils.mksha tmp_file in
  Outils.store tmp_file !dr_comms ;
  Outils.flush_tbl_fkeys !tbl_fkeys ;
  if not !bool_print_debug then Sys.remove tmp_file ;
  sha
(* ================ *)


(* ===== cmd_commit ===== *)
let compile_commit () = (* -> sha du commit *)
  let d_to_cr,f_to_cr,f_to_ch,
    d_to_rm_real,f_to_rm_real,d_to_mv_real,f_to_mv_real,
    d_to_rm_tree,f_to_rm_tree,d_to_mv_tree,f_to_mv_tree,
    _ = Pre_commit.compile_to_be () in

  let nb_d_cr = len d_to_cr
  and nb_f_cr = len f_to_cr
  and nb_f_ch = len f_to_ch
  and nb_f_rm_real = len f_to_rm_real
  and nb_f_rm_tree = len f_to_rm_tree
  and nb_d_rm_real = len d_to_rm_real
  and nb_d_rm_tree = len d_to_rm_tree
  and nb_f_mv_real = len f_to_mv_real
  and nb_f_mv_tree = len f_to_mv_tree
  and nb_d_mv_real = len d_to_mv_real
  and nb_d_mv_tree = len d_to_mv_tree in
  let nb_f_tree = nb_f_cr + nb_f_ch + nb_f_rm_tree + nb_f_mv_tree
  and nb_d_tree = nb_d_cr + nb_d_rm_tree + nb_d_mv_tree in
  let nb_f_real = nb_f_rm_real + nb_f_mv_real
  and nb_d_real = nb_d_rm_real + nb_d_mv_real in

  let f_to_ch = List.filter (fun (f,key) -> key<>Outils.mksha f) f_to_ch in
  let nb_nothing = nb_f_ch - (len f_to_ch) in

  let sha = 
  if nb_f_tree+nb_d_tree=0 
  then (printf "Nothing to commit, use mg -add first.\n"; "")
  else begin
    printf "Modification of the branch \"%s\" :\n" !branch ;
    printf "-> %d file(s) to handle (and %d dir(s)) :\n|" nb_f_tree nb_d_tree ;
    flush stdout ;
    (*=*)
    let pcommit = Outils.find_commit !branch in
    let sha = print_commit !msg pcommit
      d_to_cr d_to_mv_tree d_to_rm_tree 
      f_to_cr f_to_ch f_to_mv_tree f_to_rm_tree 
      [] [] in
    (*=*)
    printf 
    "\nDone. created : %d ; rebuilt : %d ; slightly modified : %d ; \
      unchanged : %d ; removed : %d ; moved : %d\n"
      nb_f_cr !nb_rebuilt !nb_minor nb_nothing nb_f_rm_tree nb_f_mv_tree ;
    printf "Total : %d insertions(+), %d deletions(-)\n"
      !nb_lines_add !nb_lines_del ;

    sha
  end 
  in
  
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
  sha


let cmd_commit () =
  Outils.init () ;
  Outils.rootwd () ;
  let cm = compile_commit () in
  if cm <> "" then Branch_mvt.forward !branch cm ;
  Outils.empty_file !to_be ;
  Outils.realwd ()
(* ================ *)


(* let cch = 
    open_out_gen [Open_creat ; Open_trunc] mkfile_num tmp_file in
   Donne un bug trop bizarre *)
  (*nb_lines_add := !nb_lines_add + (Array.length (Outils.readlines f)) ;*)
  (*nb_lines_del := !nb_lines_del + (Array.length (Outils.readlines f)) ;*)

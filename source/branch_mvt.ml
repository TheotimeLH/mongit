(* BACKWARD & FORWARD *)
open Root

(* === AUX === *)
(* 
 Les fichiers dont on peut avoir besoin sont sauvegardés (normalement),
 il suffit de les rajouter au tree. Excepté en cas de MINOR change où
 il faut suppr l'ancien et faire le nouveau, dont on change un peu
 all_fkeys ici aussi. 
*)
let tbl_fkeys = ref IdMap.empty
let do_d_cr d         = Tree.add_d d
let do_f_cr f key     = Tree.add_f f key
let do_d_mv op np     = Tree.add_d np     ; Tree.remove_d op
let do_f_mv op np key = Tree.add_f np key ; Tree.remove_f op
let do_d_rm d         = Tree.remove_d d
let do_f_rm f _       = Tree.remove_f f
let do_f_rb f _ nkey  = Tree.remove_f f ; Tree.add_f f nkey

let write_n_lines oc n rl =
  for _ = 1 to n do
    output_string oc (List.hd !rl ^ "\n") ;
    rl := List.tl !rl
  done 

let write_list oc l =
  write_n_lines oc (List.length l) (ref l)

let scan_n_lines cch i j =
  let l = ref [] in
  for _ = i to j do
    Outils.append l (Outils.scanf_input_line cch) 
  done ;
  (i,j,List.rev !l) 

let scan_commit_chg cch nb_chg =
  let l_add = ref []
  and l_rem = ref [] in
  for _ = 1 to nb_chg do
    Scanf.bscanf cch "%s " (function
    | "Add"    -> Scanf.bscanf cch "l-%d to l-%d (new numbering) :\n"
      (fun i j -> Outils.append l_add (scan_n_lines cch i j))
    | "Remove" -> Scanf.bscanf cch "l-%d to l-%d (old numbering) :\n"
      (fun i j -> Outils.append l_rem (scan_n_lines cch i j))
    | _ (*Modif*) -> Scanf.bscanf cch 
      "from l-%d,l-%d (old numbering) to l-%d,l-%d (new) :\n" 
      (fun i_old j_old i_new j_new -> 
        Outils.append l_rem (scan_n_lines cch i_old j_old) ;
        Scanf.bscanf cch "==================================\n" () ;
        Outils.append l_add (scan_n_lines cch i_new j_new)))
  done ;
  (!l_rem , !l_add)


(* Transformation / MINOR :
 On décompresse l'ancienne version, puis on écrit la nouvelle 
 version dans un fichier tmp. Pour ce faire on commence par 
 retirer tout ce qui ne sera pas gardé, puis on rajoute tout 
 ce qui est nouveau. Dans le commit, la façon de numéroté 
 les lignes est faite pour ça. *)
let do_f_mi f old_key new_key nb_chg cch fw =
  (* fw : bool , true si forward, false si backward, seul impact :*)
  let l_rem,l_add = scan_commit_chg cch nb_chg in 
  let l_rem,l_add = if fw then l_rem,l_add else l_add,l_rem in
  let tmp_old_file = Filename.concat !dr_files "tmp_old_file" in
  Outils.load_fn old_key !dr_files tmp_old_file ;
  let t_old = Outils.readlines tmp_old_file in
  let tmp_new_file = Filename.concat !dr_files "tmp_new_file" in
  let oc = open_out tmp_new_file in
  (* Marque de \n celles à retirer *)
  List.iter (fun (i,j,_) -> for l = i to j do t_old.(l) <- "\n" done) l_rem ;
  let l_old = ref (List.filter (fun l -> l<>"\n") (Array.to_list t_old)) in
  (* Écrit avec les insertions nécessaires : *)
  let l_add = List.sort (fun (i1,_,_) (i2,_,_) -> compare i1 i2) l_add in
  let i_new = ref 0 in
  List.iter 
  (fun (i,j,lines) -> 
    write_n_lines oc (i - !i_new) l_old ;
    i_new := j+1 ;
    write_list oc lines
  ) l_add ;
  write_list oc !l_old ;
  close_out oc ;

  assert (Outils.mksha tmp_new_file = new_key) ;
  Outils.store tmp_new_file !dr_files ;
  Outils.remove tmp_new_file ;
  tbl_fkeys := Outils.map_set_rm  old_key !branch !tbl_fkeys ;
  tbl_fkeys := Outils.map_set_add new_key !branch !tbl_fkeys ;
  do_f_rb f old_key new_key
(* ========================== *)


(* ===== FORWARD ===== *)
let forward branch cm_sha =
  Outils.branch_switch branch ;
  tbl_fkeys := Tree.load_tbl_fkeys () ;
  let tmp_commit = Filename.concat !dr_comms "tmp_commit" in
  Outils.load_fn cm_sha !dr_comms tmp_commit ;
  let cch = Scanf.Scanning.open_in tmp_commit in
  Scanf.bscanf cch "Parent commits : %_d %_s\n" () ;
  let nb_op = Scanf.bscanf cch "Nb operations : %d\n" (fun n -> n) in
  for _ = 1 to nb_op do
    Scanf.bscanf cch "%s %s "
    (fun a b -> match a,b with
    | "CREATE","DIR"     -> Scanf.bscanf cch "%s\n"       do_d_cr
    | "CREATE","FILE"    -> Scanf.bscanf cch "%s %s\n"    do_f_cr
    | "MOVE"  ,"DIR"     -> Scanf.bscanf cch "%s %s\n"    do_d_mv
    | "MOVE"  ,"FILE"    -> Scanf.bscanf cch "%s %s %s\n" do_f_mv
    | "REMOVE","DIR"     -> Scanf.bscanf cch "%s\n"       do_d_rm
    | "REMOVE","FILE"    -> Scanf.bscanf cch "%s %s\n"    do_f_rm
    | "MODIF" ,"REBUILT" -> Scanf.bscanf cch "%s %s %s\n" do_f_rb
    | _,_-> Scanf.bscanf cch "%s %s %s %d\n" (*"MODIF""MINOR"*)   
      (fun f okey nkey nb_chg -> do_f_mi f okey nkey nb_chg cch true))
  done ;
  Scanf.Scanning.close_in cch ;
  Outils.flush_tbl_fkeys !tbl_fkeys ;
  Outils.remove tmp_commit ;
  Outils.set_commit branch cm_sha ;
  Outils.branch_switch_former () ;
  print_detail "Forward : branch %s -> %s\n" branch cm_sha
(* ========================== *)


(* ===== BACKWARD ===== *)
(* Les deux fonctions sont très proches, mais puisqu'elles lisent
   chaque ligne dans le sens inverse, j'ai quand même fait 2 fct *)
let backward branch cm_sha =
  Outils.branch_switch branch ;
  tbl_fkeys := Tree.load_tbl_fkeys () ;
  let tmp_commit = Filename.concat !dr_comms "tmp_commit" in
  Outils.load_fn cm_sha !dr_comms tmp_commit ;
  let cch = Scanf.Scanning.open_in tmp_commit in
  Scanf.bscanf cch "Parent commits : %_d %_s\n" () ;
  let nb_op = Scanf.bscanf cch "Nb operations : %d\n" (fun n -> n) in
  for _ = 1 to nb_op do
    Scanf.bscanf cch "%s %s "
    (fun a b -> match a,b with
    | "CREATE","DIR"     -> Scanf.bscanf cch "%s\n"       do_d_rm
    | "CREATE","FILE"    -> Scanf.bscanf cch "%s %s\n"    do_f_rm
    | "MOVE"  ,"DIR"     -> Scanf.bscanf cch "%s %s\n"    (fun op np -> do_d_mv np op)
    | "MOVE"  ,"FILE"    -> Scanf.bscanf cch "%s %s %s\n" (fun op np key -> do_f_mv np op key)
    | "REMOVE","DIR"     -> Scanf.bscanf cch "%s\n"       do_d_cr
    | "REMOVE","FILE"    -> Scanf.bscanf cch "%s %s\n"    do_f_cr
    | "MODIF" ,"REBUILT" -> Scanf.bscanf cch "%s %s %s\n" (fun f ok nk -> do_f_rb f nk ok)
    | _,_-> Scanf.bscanf cch "%s %s %s %d\n" (*"MODIF""MINOR"*)   
      (fun f okey nkey nb_chg -> do_f_mi f nkey okey nb_chg cch false))
  done ;
  Scanf.Scanning.close_in cch ;
  Outils.flush_tbl_fkeys !tbl_fkeys ;
  Outils.remove tmp_commit ;
  Outils.set_commit branch cm_sha ;
  Outils.branch_switch_former () ;
  print_detail "Forward : branch %s -> %s\n" branch cm_sha
(* ========================== *)


open Printf
open Root

(* ===== LIST ===== *)
let list_br () =
  let a = Sys.readdir !dr_brnch in
  let l = ref [] in
  Array.iter (fun br -> if br<>"HEAD" then Outils.append l br) a ;
  !l

let cmd_list () =
  Outils.init () ;
  printf "List of branches : " ;
  List.iter 
    (fun br -> if br <> !branch then printf "%s " br 
    else (printf "\x1B[34m%s\x1B[97m " br)) (list_br ()) ;
  print_newline ()
(* ================= *)


(* ===== CREATE ===== *)
let copy_tree br1 br2 =
  let op = Outils.sha_name (Outils.with_branch br1 "")
  and np = Outils.sha_name (Outils.with_branch br2 "") in
  let rec aux op np dir =
    let op = Filename.concat !dr_trees op
    and np = Filename.concat !dr_trees np in
    let ic = Scanf.Scanning.open_in op
    and oc = open_out np in
    try while true do
      Scanf.bscanf ic "%s %s %s\n" 
      (fun t bn k -> fprintf oc "%s %s %s\n" t bn
      (if t="file" then k else (
       let subdir = Filename.concat dir bn in
       let np = (Outils.sha_name (Outils.with_branch br2 subdir)) in
       aux k np subdir; np)))
    done with | End_of_file -> Scanf.Scanning.close_in ic ; close_out oc
  in
  aux op np ""

let duplicate_keys br1 br2 = (*-> files/all_fkeys*)
  let tbl_fkeys = Tree.load_tbl_fkeys () in
  let fct = (fun st -> if IdSet.mem br1 st then IdSet.add br2 st else st) in
  let new_tbl = IdMap.map fct tbl_fkeys in
  Outils.print_tbl_fkeys new_tbl


let create br1 new_br =
  if List.mem new_br (list_br ()) then 
  ( eprintf "There already exists a branch called %s.\n" new_br ; exit 1) ;
  copy_tree br1 new_br ;
  duplicate_keys br1 new_br ;
  Outils.set_commit new_br (Outils.find_commit br1)


let cmd_create new_br =
  Outils.init () ;
  create !branch new_br
(* ================= *)


(* ===== SWITCH ===== *)
let cmd_switch br =
  Outils.init () ;
  if not (List.mem br (list_br ())) then
  (eprintf "There is no branch called %s.\n" br ; exit 1) ;
  let ic = open_in !to_be in
  begin try ignore (input_line ic) ; (* devrait raise EOF *)
    eprintf "The \"to_be_commited\" file have been erased \
             to avoid problems with the branch switch.\n" ;
    close_in ic ;
    Outils.empty_file !to_be
  with | End_of_file -> () end ;
  Outils.branch_switch br
(* ================= *)


(* ===== GRAPH ===== *)
(* On construit 2 graphes, g1 qui à un commit associe l'ens 
   de ses parents et g2 qui donne l'ens de ses enfants. *)
let make_commit_graph commits =
  List.fold_left 
  (fun (g1,g2) cm -> 
    let tmp_commit = Filename.concat !dr_comms "tmp_commit" in
    Outils.load_fn cm !dr_comms tmp_commit ;
    let ic = Scanf.Scanning.open_in tmp_commit in
    let list_pcm = Scanf.bscanf ic "Parent commits : %d " 
    (fun n -> let l = ref [] in
      for _ = 1 to n do 
        Scanf.bscanf ic "%s " (Outils.append l)
      done ; !l
    ) in
    Scanf.Scanning.close_in ic ;
    Outils.remove tmp_commit ;
    (IdMap.add cm (Outils.set_of_list list_pcm) g1  ,
     List.fold_right (fun pcm -> Outils.map_set_add pcm cm) list_pcm g2)
  ) (IdMap.empty,IdMap.empty) commits


let cmd_graph () =
  Outils.init () ;
  let oc = open_out "branches.dot" in
  fprintf oc "digraph branches_graph{\nrankdir=LR;\n" ;
  let commits = Outils.list_sha !dr_comms in
  let gr = fst (make_commit_graph commits) in
  let n = ref (List.length commits) in
  let tbl_num = ref (IdMap.singleton "none" !n) in
  fprintf oc "%d [label=\"none\"];\n" !n;
  List.iteri (fun i cm -> tbl_num := IdMap.add cm i !tbl_num) commits ;
  List.iter
  (fun cm ->
    let num  = IdMap.find cm  !tbl_num in
    fprintf oc "%d [label=\"%s\"];\n" num (Outils.short cm) ; 
    IdSet.iter 
      (fun pcm -> fprintf oc "%d -> %d;\n" (IdMap.find pcm !tbl_num) num)
      (IdMap.find cm gr)
  ) commits ;

  let branches = list_br () in
  List.iter
  (fun br ->
    incr n ;
    if br = !branch 
    then fprintf oc "%d [label=\"%s\",color=red];  \n" !n br
    else fprintf oc "%d [label=\"%s\",color=blue]; \n" !n br ;
    let ic = open_in (Filename.concat !dr_brnch br) in
    Scanf.sscanf (input_line ic) "last commit : %s"
    (fun cm -> 
      let num = IdMap.find cm !tbl_num in
      fprintf oc "%d -> %d [arrowhead=none,arrowtail=inv];\n" num !n) ;
    close_in ic
  ) branches ;

  fprintf oc "}" ;
  close_out oc ;
  Outils.use_graphviz "branches"
(* ================= *)


(* ===== FORWARD / BACKWARD ====== *)
let move_branch sens br nb_pas =
  let commits = Outils.list_sha !dr_comms in
  let gr = 
    (if sens then snd else fst) 
    (make_commit_graph commits) in
  let mvt_fct,mvt = if sens 
    then (Branch_mvt.forward ,"forward" )
    else (Branch_mvt.backward,"backward") in
  let cm = ref (Outils.find_commit br) in
  for _ = 1 to nb_pas do
    if !cm = "None" then (eprintf "Reached the root state.\n" ; exit 0) ;
    let st_next = IdMap.find !cm gr in
    match IdSet.elements st_next with
    | [] -> ()
    | [cm_next] -> 
      if cm_next<>"None" then (mvt_fct br cm_next ; cm := cm_next)
    | l -> printf
      "The branch %s cannot be moved %s after \"%s...\" \
       because several commits are possible :\n%s\n"
       br mvt (Outils.short !cm) (String.concat "\n" l) ;
       let cm_next = ref "" in
       while !cm_next<>"stop" && not (List.mem !cm_next l) do
        printf "You can write \"stop\" to end the migration here, \
                and use \"mg -cat_commit <sha>\" and/or \"mg -branch \
                -graph\" to thought.\n" ;
        cm_next := read_line ()
       done ;
       if !cm_next="stop" then (printf "Stopped.\n";exit 0)
       else if !cm_next="none" then exit 0
       else (mvt_fct br !cm_next ; cm := !cm_next)
  done

let cmd_forward nb_pas = 
  Outils.init () ;
  move_branch true !branch nb_pas

let cmd_backward nb_pas = 
  Outils.init () ;
  move_branch false !branch nb_pas
(* ================= *)




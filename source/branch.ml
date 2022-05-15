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
  let last_commit = Outils.find_commit br1 in
  let oc = open_out (Filename.concat !dr_brnch new_br) in
  fprintf oc "last commit : %s\n" last_commit ;
  close_out oc 


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
    eprintf "The \"to_be_commited\" file have been erased to avoid problems with the branch switch.\n" ;
    close_in ic ;
    Outils.empty_file !to_be
  with | End_of_file -> () end ;
  Outils.init_file (Filename.concat !dr_brnch "HEAD") (br^"\n")
(* ================= *)


(* ===== GRAPH ===== *)
let cmd_graph () =
  Outils.init () ;
  let oc = open_out "branches.dot" in
  fprintf oc "digraph branches_graph{\nrankdir=LR;\n" ;
  let commits = Outils.list_sha !dr_comms in
  let n = ref (List.length commits) in
  let tbl_num = ref (IdMap.singleton "none" !n) in
  fprintf oc "%d [label=\"none\"];\n" !n;
  List.iteri (fun i cm -> tbl_num := IdMap.add cm i !tbl_num) commits ;
  List.iter
  (fun cm ->
    let num = IdMap.find cm !tbl_num in
    fprintf oc "%d [label=\"%s\"];\n" num (String.sub cm 0 4) ;
    (* Il faut charger le commit dans un fichier tmp : *)
    let tmp_commit = Filename.concat !dr_comms "tmp_commit" in
    let tmp_ch = open_out tmp_commit in
    Outils.load cm !dr_comms tmp_ch ;
    close_out tmp_ch ;
    let ic = Scanf.Scanning.open_in tmp_commit in
    Scanf.bscanf ic "Parent commits : %s\n" 
    (fun pcm -> 
      let pnum = IdMap.find pcm !tbl_num in
      fprintf oc "%d -> %d;\n" pnum num) ;
    Scanf.Scanning.close_in ic ;
    Outils.remove tmp_commit
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

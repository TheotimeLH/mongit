open Printf
open Root

(* ===== CREATE ===== *)
let cmd_create new_br =
  Outils.init () ;
  Outils.rootwd () ;
  Tree.copy !branch new_br ;
  let last_commit = Outils.find_commit () in
  let oc = open_out (Filename.concat !dr_brnch new_br) in
  fprintf oc "last commit : %s\n" last_commit ;
  close_out oc ;
  Outils.realwd ()
(* ================= *)


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


(* ===== SWITCH ===== *)
let cmd_switch br =
  Outils.init () ;
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

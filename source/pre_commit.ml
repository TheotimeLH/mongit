(* RESTORE / ADD / RENAME / REMOVE *)
(* ~ PRE-COMMIT operations *)
(* Everything works both for files and directories. *)

open Root
open Printf

(* ===== PRE-COMMIT : ADD/MINUS/REMOVE/MOVE ===== *)
(* Cette commande sert juste à modifier la liste "to_be_commited".
   Encore une fois, on peut add/minus/remove un dossier. Auquel cas
   seul le nom du dossier sera inscrit. *)
let pre_commit_cmd ope f = (* ope = add | minus | remove*)
  Outils.init () ;
  if ope<>"remove" || not !only_on_repo then Outils.exists_chk f ;
  let ope = 
    if ope<>"remove" then ope
    else sprintf "remove_%B" !only_on_repo 
  in
  let oc = open_out_gen [Open_creat ; Open_append] mkfile_num !to_be in
  let rf = Outils.rootpath f in
  if rf = "" (* root *)
  then fprintf oc "all _ %s\n" ope
  else fprintf oc "%s _ %s\n" ope (Outils.rootpath f) ;
  close_out oc

let cmd_move = function
  | [oldpath;newpath] ->
    Outils.init () ;
    if not !only_on_repo then Outils.exists_chk oldpath ;
    let oc = open_out_gen [Open_creat ; Open_append] mkfile_num !to_be in
    let r_old = Outils.rootpath oldpath
    and r_new = Outils.rootpath newpath in
    if r_old="" || r_new="" then
    ( eprintf "You can't move the whole directory like that.\n" ;
      exit 1 )
    else if (r_old <> r_new) && (Tree.dont_overwrite_chk !only_on_repo r_new)
    then fprintf oc "move_%B %s %s\n" !only_on_repo r_old r_new ;
    close_out oc
  | _ -> 
      eprintf "To move a file/dir with mg you must use :\
        \"mg [-only_on_repo] -move <old-path> <new-path>\"\n" ;
      exit 1
(* ================ *)


(* ===== PRE-COMMIT : compile todo list ===== *)
(* Traitement de to_be_commited, renvoie 7 listes :
   - Les fichiers à créer : f_to_cr
         ceux à supprimer : f_to_rm 
         ceux à déplacer  : f_to_mv 
         ceux à modifier  : f_to_ch
   - Les dossiers à créer : d_to_cr (dans le tree) 
         ceux à supprimer : d_to_rm 
         ceux à déplace   : d_to_mv
   Accessoirement renvoie tbl_files : key IdMap.t
   Utile :
     Pour le status
     Pour le commit (~ fait la todo_list)
*)
let compile_to_be () = (* cwd = root *)
  print_debug "demande à lire to_be\n" ;
  let f_to_rm_real = ref []  and f_to_rm_tree = ref [] 
  and f_to_mv_real = ref []  and f_to_mv_tree = ref [] 
  and d_to_rm_real = ref []  and d_to_rm_tree = ref [] 
  and d_to_mv_real = ref []  and d_to_mv_tree = ref []  
  and f_to_cr = ref [] and f_to_ch = ref []
  and d_to_cr = ref [] in
  let l_add_f = ref [] in
  let tbl_files = Tree.load_tbl_files () in

  (* ==== FONCTIONS AUXILIAIRES : ==== *)
  (* usefull *)
  let apply_rec d dr fct = (* dr = d but rootpath*)
    Array.iter
    (fun sub -> 
      if !include_secret || sub.[0] <> '.' 
      then fct (Filename.concat dr sub))
    (Sys.readdir d)
  in
  (* == ADD/MINUS == *)
  let add_f b f = if b
    then l_add_f := f :: !l_add_f (*add*)
    else l_add_f := Outils.list_rm_fst_occ f !l_add_f (*minus*)
  in
  let rec add_d b d = 
    if b then (*add*)
      begin try ignore (Tree.find_key_d d)
      with | Not_in_the_tree -> d_to_cr := d :: !d_to_cr 
    end
    else d_to_cr := Outils.list_rm_fst_occ d !d_to_cr ; (*minus*)
    apply_rec d d (add_d_or_f b) 
  and add_d_or_f b df =
    if Sys.is_directory df then add_d b df else add_f b df
  in
  let add b df =
    Outils.exists_chk df ;
    add_d_or_f b df 
  in


  (* == REMOVE == *)
  let remove_real df =
    Outils.exists_chk df ;
    if Sys.is_directory df 
    then Outils.append d_to_rm_real df
    else Outils.append f_to_rm_real df 
  in

  let remove_tree df =
    try 
      let ldir,lfiles = Tree.enumerate_unk df in
      Outils.extend d_to_rm_tree ldir ;
      Outils.extend f_to_rm_tree lfiles
    with | Not_in_the_tree -> () (* ignore *)   
  in

  let remove only_repo df =
    if not only_repo then remove_real df ;
    remove_tree df 
  in


  (* == MOVE == *) (* op : oldpath ; np : newpath *)
  let move_real op np = 
    Outils.exists_chk op ;
    if Sys.is_directory op
    then Outils.append d_to_mv_real (op,np)
    else Outils.append f_to_mv_real (op,np)
  in

  let move_tree op np =
    try
      let n = String.length op in
      let op_to_np df = np ^ (String.sub df n (String.length df - n)) in
      let ldir,lfiles = Tree.enumerate_unk op in
      Outils.extend d_to_mv_tree (List.map (fun d -> (d,op_to_np d)) ldir) ;
      Outils.extend f_to_mv_tree (List.map (fun (f,k) -> ((f,k),op_to_np f)) lfiles)
    with | Not_in_the_tree -> () (* ignore *)
  in

  let move only_repo op np =
    if not only_repo then move_real op np ;
    move_tree op np
  in

  
  (* ==== SCAN ==== *)
  let ic = Scanf.Scanning.open_in !to_be in
  begin try while true do
    Scanf.bscanf ic "%s %s %s\n"
    (
    fun a df df2 -> 
      print_debug "parse qlqch\n" ;
      let fct_to_use = match a,df2 with
      | "all","add"    | "add",_    -> add true
      | "all","minus"  | "minus",_  -> add false
      | "all","remove_true"  | "remove_true",_  -> remove true
      | "all","remove_false" | "remove_false",_ -> remove false
      | "move_true",_  -> move true df
      | "move_false",_ -> move false df
      | _,_ -> failwith "external modif of to_be_commited file maked it unreadable\n"
      in
      if a="all" then apply_rec "." "" fct_to_use
      else fct_to_use df2
    )
  done with | End_of_file -> () end ;
  Scanf.Scanning.close_in ic ;

  (* === *)
  List.iter
    (fun f -> match IdMap.find_opt f tbl_files with
      | None -> f_to_cr := f :: !f_to_cr
      | Some key -> f_to_ch := (f,key) :: !f_to_ch
    ) !l_add_f ;

  Outils.list_uniq !d_to_cr ,
  Outils.list_uniq !f_to_cr ,
  Outils.list_uniq !f_to_ch ,
  Outils.list_uniq !d_to_rm_real ,
  Outils.list_uniq !f_to_rm_real ,
  Outils.list_uniq !d_to_mv_real ,
  Outils.list_uniq !f_to_mv_real ,
  Outils.list_uniq !d_to_rm_tree ,
  Outils.list_uniq !f_to_rm_tree ,
  Outils.list_uniq !d_to_mv_tree ,
  Outils.list_uniq !f_to_mv_tree ,
  tbl_files

(* ================ *)



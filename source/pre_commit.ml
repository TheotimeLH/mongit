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
  let oc = open_out_gen [Open_creat ; Open_append] mkfile_num
    (Filename.concat !repo "to_be_commited") in
  let rf = Outils.rootpath f in
  if rf = "" (* root *)
  then fprintf oc "all _ %s\n" ope
  else fprintf oc "%s _ %s\n" ope (Outils.rootpath f) ;
  close_out oc

let cmd_move = function
  | [oldpath;newpath] ->
    Outils.init () ;
    let oc = open_out_gen [Open_creat ; Open_append] mkfile_num
      (Filename.concat !repo "to_be_commited") in
    let r_old = Outils.rootpath oldpath
    and r_new = Outils.rootpath newpath in
    if r_old="" || r_new="" then
    ( eprintf "You can't move the whole directory like that.\n" ;
      exit 1 )
    else fprintf oc "move %s %s" r_old r_new ;
    close_out_oc
  | _ -> 
      eprintf "To move a file/dir with mg you must use :\
        \"mg -move <old-path> <new-path>\"\n" ;
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
  let to_be    = Filename.concat !repo "to_be_commited" in
  let dr_trees = Filename.concat !repo "trees" in
  let f_to_rm = ref []    and f_to_mv = ref [] 
  and f_to_cr = ref []    and f_to_ch = ref []
  and d_to_cr = ref []    and d_to_rm = ref []
  and d_to_mv = ref []    in
  let l_add_f = ref [] in
  let tbl_files = Tree.load_tbl_files () in

  (* ==== FONCTIONS AUXILIAIRES : ==== *)
  (* usefull *)
  let fct_on_d d fct =
    Array.iter
    (fun sub -> 
      if !include_secret || sub.[0] <> '.' 
      then fct (Filename.concat d sub))
    (Sys.readdir d)
  in
  (* == ADD/MINUS == *)
  let add_f b f = 
    if b then l_add_f := f :: !l_add_f (*add*)
    else l_add_f := Outils.list_rm_fst_occ f !l_add_f (*minus*)
  in
  let add_d b d = 
    if b then (*add*)
      begin try ignore (find_key_d d)
      with | Not_in_the_tree -> d_to_cr := d :: !d_to_cr end
    else d_to_cr := Outils.list_rm_fst_occ d !d_to_cr ; (*minus*)
    fct_on_d d (add_d_or_f b)
  and add_d_or_f b df =
    if Sys.is_directory df then add_d b df else add_f b df
  in
  let add b df =
    Outils.exists_chk df ;
    add_d_or_f b df 
  in


  (* == REMOVE == *)
  let remove_f f = (f_to_rm := f :: !f_to_rm) in
  let remove_d d =
    begin try ignore (find_key_d d) ; d_to_rm := d :: !d_to_rm
    with | Not_in_the_tree -> () end ;
    fct_on_d d remove_d_or_f
  and remove_d_or_f df =
    if Sys.is_directory df then remove_d df else remove_f df
  in
  let remove df =
    if Sys.file_exists df then remove_d_or_f df
  in


  (* == MOVE == *) (* op : oldpath ; np : newpath *)
  let move_f op np = (f_to_mv := (op,np) :: !f_to_mv) in
  let move_d op np = 
    d_to_mv := (op,np) :: !d_to_mv ;
    Array.iter
    (fun sub -> 
      if !include_secret || sub.[0] <> '.' 
      then move_d_or_f (Filename.concat op sub) (Filename.concat np sub))
    (Sys.readdir op)
  and move_d_or_f op np =
    if Sys.is_directory op then move_d op np else move_f op np
  in
  let move op np =
    Outils.exists_chk op ;
    if Sys.file_exists np then
    ( eprintf "move %s %s ignored because %s already exists, we avoid overwriting\n"
      op np np )
    else move_d_or_f op np
  in
  
  (* ==== SCAN ==== *)
  let ic = Scanf.Scanning.open_in to_be in
  begin try while true do
    Scanf.bscanf ic "%s %s %s\n"
    (
    fun a df df2 -> 
      let fct_to_use = match a,df2 with
      | "all","add"    | "add",_    -> add true
      | "all","minus"  | "minus",_  -> add false
      | "all","remove" | "remove",_ -> remove
      | "move",_ -> move_d_or_f df
      in
      if a="all" then fct_on_d "." fct_to_use
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

  !f_to_cr,!f_to_ch,!f_to_rm,!f_to_mv,!d_to_cr,!d_to_ch,!d_to_rm,tbl_files
(* ================ *)





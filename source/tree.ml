open Printf
open Root

(* ===== ADD ===== *)
let rec add_f f key = 
  (* /!\ non déjà présent et en rootpath !*)
  let dir = Filename.dirname f |> Outils.np in
  let fnd = Outils.with_branch dir in
  let hdir = Filename.concat !dr_trees (Outils.sha_name fnd) in 
  if not (Sys.file_exists hdir) then add_d dir ;
  let oc_dir = open_out_gen [Open_append] 0 hdir in
  output_string oc_dir 
    (sprintf "file %s %s\n" (Filename.basename f) key) ;
  close_out oc_dir

and add_d d =
  let fnd = Outils.with_branch d in
  let key = Outils.sha_name fnd in
  let hf = Filename.concat !dr_trees key in
  if not (Sys.file_exists hf) then begin
  let parent = Filename.dirname d |> Outils.np in
  let fndp = Outils.with_branch parent in 
  let hparent = Filename.concat !dr_trees (Outils.sha_name fndp) in
  if not (Sys.file_exists hparent) then add_d parent ;
  let oc_par = open_out_gen [Open_append] 0 hparent in
  output_string oc_par 
    (sprintf "dir %s %s\n" (Filename.basename d) key) ;
  close_out oc_par ;
  Outils.empty_file hf
  end
(* ================ *)


(* ===== LS ===== *)
let cmd_ls () =
  Outils.init () ;
  let oc = open_out "repo_tree.dot" in
  fprintf oc "digraph repo_tree_branch_%s{\n" !branch;
  let num_max = ref 0 in
  let rec read name key num =
    print_debug "Read tree %s cle %s \n" name key ;
    fprintf oc "%d [label=\"/%s\"];\n" num name ;
    let f = Filename.concat !dr_trees key in
    Outils.exists_chk f ;
    let ic = Scanf.Scanning.open_in f in
    begin try while true do
      incr num_max ;
      Scanf.bscanf ic "%s %s %s\n"
      (fun t s nk ->
        fprintf oc "%d -> %d;\n" num !num_max ;
        if t = "dir" then read s nk !num_max
        else fprintf oc "%d [label=\"%s\"];\n" !num_max s)
    done with | End_of_file -> () end ;
    Scanf.Scanning.close_in ic
  in
  read "" (Outils.sha_name (Outils.with_branch "")) 0;
  fprintf oc "}" ;
  close_out oc ;
  let ret = Sys.command 
    "dot -Tpdf repo_tree.dot > repo_tree.pdf && evince repo_tree.pdf" in
  if ret <> 0 then
    ( eprintf "(controled) problem with dot cmd\n" ; exit 1) ;
  if not !bool_print_debug then
    (Sys.remove "repo_tree.pdf" ;
     Sys.remove "repo_tree.dot")
(* ================ *)


(* ===== FIND KEY  ===== *)
let find_key_d d = (* en rootpath *)
  let key = Outils.sha_name (Outils.with_branch d) in
  let dt = Filename.concat !dr_trees key in
  if not (Sys.file_exists dt) then raise Not_in_the_tree
  else key

let find_key_df df = 
(* À utiliser pour un file ou quand on ne sait pas.
   Cherche dans le tree parent, et ainsi on peut savoir
   si un df suppr était un dossier ou un file. *)
  let dir = Filename.dirname df |> Outils.np in
  let dkey = find_key_d dir in
  let tree = Filename.concat !dr_trees dkey in
  let bn = Filename.basename df in
  let ic = Scanf.Scanning.open_in tree in
  let rec read () =
    Scanf.bscanf ic "%s %s %s\n"
    (fun t s nk ->
      if s=bn then 
      ( Scanf.Scanning.close_in ic ; (t="file" , nk))
      else read ())
  in
  try read ()
  with | End_of_file -> 
    Scanf.Scanning.close_in ic ; raise Not_in_the_tree

(* ================ *)


(* ===== ENUMERATE ===== *)
let enumerate d key =
  let l = ref [] in
  let rec read dir key =
    let tree = Filename.concat !dr_trees key in
    Outils.exists_chk tree ;
    let ic = Scanf.Scanning.open_in tree in
    begin try while true do
      Scanf.bscanf ic "%s %s %s\n"
      (fun t s nk ->
        if t="dir" then read (Filename.concat dir s) nk
        else if !include_secret || s.[0] <> '.' 
        then l := (Filename.concat dir s,nk) :: !l)
    done with | End_of_file -> () end ;
  in
  read d key ;
  !l

let enumerate_unk df =
  print_debug "enumerate df = %s\n" df ;
  if df="" then enumerate "" (find_key_d "")
  else 
    let is_f,key = find_key_df df in
    if is_f then [(df,key)]
    else enumerate df key

let load_tbl_files () =
  List.fold_left
    (fun tbl (f,key) -> IdMap.add f key tbl)
    IdMap.empty
    (enumerate "" (find_key_d ""))
(* ================ *)


(* ===== REMOVE ===== *)
let remove_f df =
  print_debug "Try to remove : %s from the tree\n" df ;
  let dir = Filename.dirname df |> Outils.np in
  let fnd = Outils.with_branch dir in
  let hdir = Filename.concat !dr_trees (Outils.sha_name fnd) in 
  let bn = Filename.basename df in
  if Sys.file_exists hdir then begin
  (* Je n'ai pas trouvé mieux que de réécrire tout le fichier *)
    let al = Outils.readlines hdir in
    let oc = open_out hdir in
    Array.iter 
      (fun s -> Scanf.sscanf s "%s %s %s" 
      (fun _ fn _ -> if fn<>bn then output_string oc (s^"\n")))
      al ;
    close_out oc
  end ;
  try ignore (find_key_df df) ; print_debug "QUOIIII on a trouvé : %s \n" df
  with | Not_in_the_tree -> ()

let rec erase key =
  let hf = Filename.concat !dr_trees key in
  let l_rm_next = ref [] in
  if Sys.file_exists hf then begin
    let al = Outils.readlines hf in
    Array.iter 
      (fun s -> Scanf.sscanf s "%s %s %s"
      (fun t _ k -> if t="dir" then l_rm_next := k :: !l_rm_next))
      al ;
    Sys.remove hf
  end ;
  List.iter erase !l_rm_next


let remove_d d =
  remove_f d ; (* vis à vis du parent *)
  (* Maintenant il faut supprimer le sub_tree en soit *)
  let fnd = Outils.with_branch d in
  let key = Outils.sha_name fnd in
  erase key
(* ================ *)



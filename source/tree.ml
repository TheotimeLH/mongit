open Printf
open Root

(* ===== ADD ===== *)
let rec add_file dr_trees f key = 
  (* /!\ non déjà présent et en rootpath !*)
  let dir = Filename.dirname f in
  let dir = if dir="." then "" else dir in
  let hdir = Filename.concat dr_trees (Outils.sha_name dir) in 
  if not (Sys.file_exists hdir) then add_dir dr_trees dir ;
  let oc_dir = open_out_gen [Open_append] 0 hdir in
  output_string oc_dir 
    (sprintf "file %s %s\n" (Filename.basename f) key) ;
  close_out oc_dir

and add_dir dr_trees dir =
  let hf = Filename.concat dr_trees (Outils.sha_name dir) in
  if not (Sys.file_exists hf) then begin
  let parent = Filename.dirname dir in
  let parent = if parent = "." then "" else parent in
  let hparent = Filename.concat dr_trees (Outils.sha_name parent) in
  if not (Sys.file_exists hparent) then add_dir dr_trees parent ;
  let oc_par = open_out_gen [Open_append] 0 hparent in
  output_string oc_par 
    (sprintf "dir %s %s\n" (Filename.basename dir) (Outils.sha_name dir)) ;
  close_out oc_par ;
  Outils.empty_file hf
  end
(* ================ *)


(* ===== LS ===== *)
let cmd_ls () =
  let repo = Outils.repo_find_chk () in
  let dr_trees = Filename.concat repo "trees" in
  let oc = open_out "repo_tree.dot" in
  fprintf oc "digraph repo_tree{\n" ;
  let num_max = ref 0 in
  let rec read name key num =
    fprintf oc "%d [label=\"/%s\"];\n" num name ;
    let f = Filename.concat dr_trees key in
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
  read "" (Outils.sha_name "") 0;
  fprintf oc "}" ;
  close_out oc ;
  let ret = Sys.command 
    "dot -Tpdf repo_tree.dot > repo_tree.pdf && evince repo_tree.pdf" in
  if ret <> 0 then
    ( eprintf "(controled) problem with dot cmd\n" ; exit 1) ;
  if not !bool_print_debug then
    (Sys.remove "repo_tree.pdf" ;
     Sys.remove "repo_tree.dot")

  (* Puis on compile en pdf, mais attention, je ne veux pas
     arrêter pour autant, donc je fork
  begin match Unix.fork () with
    | 0 ->
      Unix.execvpe "dot" args (Unix.environment ())
    | _ ->
      let _,status = Unix.wait () in
      match status with
      | WEXITED i -> 
          if i<>0 then
          ( eprintf "(controled) problem with dot cmd" ;
            exit 1)
      | _ -> failwith "problem with external dot cmd\n"
  end *)
(* ================ *)

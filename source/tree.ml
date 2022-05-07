open Printf
open Root

(* ===== ADD ===== *)
let rec add_file dr_trees f = 
  (* /!\ non déjà présent !*)
  let dir = Filename.dirname f in
  let hdir = Filename.concat dr_trees (Outils.sha_name dir) in 
  if not (Sys.file_exists hdir) then add_dir dr_trees dir ;
  let oc_dir = open_out_gen [Open_append] 0 hdir in
  output_string oc_dir (sprintf "file %s\n" (Filename.basename f)) ;
  close_out oc_dir

and add_dir dr_trees dir =
  let parent = Filename.dirname dir in
  let hparent = Filename.concat dr_trees (Outils.sha_name parent) in
  if not (Sys.file_exists hparent) then add_dir dr_trees parent ;
  let oc_par = open_out_gen [Open_append] 0 hparent in
  output_string oc_par (sprintf "dir %s\n" (Outils.sha_name dir)) ;
  close_out oc_par ;
  Outils.empty_file (Filename.concat dr_trees (Outils.sha_name dir))
(* ================ *)


(* ===== LS ===== *)
let cmd_ls () =
  let repo = Outils.repo_find_chk () in
  let dr_trees = Filename.concat repo "trees" in
  let oc = open_out "repo_tree.dot" in
  fprintf oc "digraph repo_tree{\n" ;
  let rec read key =
    let f = Filename.concat dr_trees key in
    Outils.exists_chk f ;
    let ic = Scanf.Scanning.open_in (Filename.concat dr_trees key) in
    begin try while true do
      Scanf.bscanf ic "%s %s\n"
      (fun t nk ->
        fprintf oc "\"%s\" -> \"%s\";\n" key nk ;
        if t = "dir" then read nk)
    done with | End_of_file -> () end ;
    Scanf.Scanning.close_in ic
  in
  read (Outils.sha_name (Filename.dirname repo)) ;
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

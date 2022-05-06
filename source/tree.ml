open Printf

let rec add_file dr_trees f = 
  (* /!\ apres Unix.realpath et non déjà présent !*)
  let dir = Filename.dirname f in
  let hdir = Filename.concat dr_trees (Outils.sha_name dir) in 
  if not (Sys.file_exists hdir) then add_dir dir ;
  let oc_dir = open_out_gen [Open_append] 0 hdir in
  output_string oc_dir (sprintf "file %s\n" f) ;
  close_out oc_dir

and add_dir dr_trees dir =
  let parent = Filename.dirname dir in
  let hparent = Filename.concat dr_trees (Outils.sha_name parent) in
  if not (Sys.file_exists hparent) then add_dir parent ;
  let oc_par = open_out_gen [Open_append] 0 hparent in
  output_string oc_par (sprintf "dir %s\n" (Outils.sha_name dir)) ;
  close_out oc_par ;
  Outils.empty_file (Filename.concat dr_trees (Outils.sha_name dir))


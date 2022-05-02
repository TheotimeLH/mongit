open Root

let aff_qlqch str =
(* TEST DEBUG *)
(*print_debug "Affiche du debug \n" ; *)
(* TEST FILEPERM *)
(*let file = "../sysnum/.git" in
  let st = Unix.stat file in
  Printf.printf "num de perm de %s : %d \n" file st.st_perm *)
(* TEST repo_find 
  let s = Outils.repo_find () in
  Printf.printf "%s" s ; *)
  print_debug "Fin de la cmd avec : %s\n" str ;
  

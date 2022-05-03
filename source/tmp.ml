open Root

let aff_qlqch str =
(* TEST DEBUG *)
(*print_debug "Affiche du debug \n" ; *)
(* TEST FILEPERM *)
(* TEST repo_find 
  let s = Outils.repo_find () in
  Printf.printf "%s" s ; *)
(*let file = "../sysnum/.git" in
  let st = Unix.stat file in
  Printf.printf "num de perm de %s : %d \n" file st.st_perm *)
(* TEST Hash
  let key,rest = Outils.hash_file 
    "/home/theotime/Documents/Projets/mongit/source/tmp.ml" in 
  Printf.printf "key : %s , rest : %s\n" key rest ; *)
  print_debug "Fin de la cmd avec : %s\n" str ;
  

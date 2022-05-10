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
(*  print_debug "Test de rootpath : %s\n" (Outils.rootpath "patate") ; *)
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
  print_debug "Fin de la cmd avec : %s\n" str ;

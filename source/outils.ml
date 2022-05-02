open Root

(* ===== REMOVE ===== *)
let rec remove path = 
  if Sys.file_exists path
  then match Sys.is_directory path with
  | true ->
    Sys.readdir path |>
    Array.iter (fun name -> remove (Filename.concat path name));
    Unix.rmdir path
  | false ->
    Sys.remove path
(* ================== *)


(* ===== FIND THE REPO ===== *)
let repo_find_path path =
  let rec aux p =
    let mg = Filename.concat path "/.mongit" in
    if Sys.file_exists mg then mg
    else 
      let parent = Filename.dirname p in
      if p = parent then raise No_repo
      else aux parent
  in
  aux (Unix.realpath path)

let repo_find () = repo_find_path "."
(* ================== *)
  


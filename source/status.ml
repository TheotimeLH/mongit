open Printf
open Root

(* ===== PRINT TO_BE ===== *)
let l_to_be = ref []
let print_to_be repo = (* avec cwd = root *)
  (* Fonction extraite du code de Commit.mk_todo_list *)
  let l = ref [] in
  (* add *)
  let add_f = fun f -> l := f :: !l in
  let rec add_d d = Array.iter 
    (fun df -> if df.[0] <> '.' then add_d_or_f (Filename.concat d df)) 
    (Sys.readdir d)
  and add_d_or_f df =
    if Sys.is_directory df then add_d df else add_f df
  in
  (* minus *)
  let minus_f = fun f -> l := Outils.list_rm_fst_occ f !l in
  let rec minus_d d = Array.iter 
    (fun df -> if df.[0] <> '.' then minus_d_or_f (Filename.concat d df)) 
    (Sys.readdir d)
  and minus_d_or_f df =
    if Sys.is_directory df then minus_d df else minus_f df
  in
  (* Scan to_be_commited *)
  let to_be = Filename.concat repo "to_be_commited" in
  if not (Sys.file_exists to_be)
  then printf "[COMMIT] There are no files waiting for a -commit.\n"
  else begin
  (* SCAN *)
  let ic = Scanf.Scanning.open_in to_be in
  begin try while true do
    Scanf.bscanf ic "%s %s\n"
    (fun a df -> 
      if a="all" then begin
        Array.iter 
          (fun sub -> if sub.[0] <> '.' then 
           if df = "add" then add_d_or_f sub else minus_d_or_f sub) 
          (Sys.readdir ".")
      end else begin
      Outils.exists_chk df ;
      if a="add" then add_d_or_f df else minus_d_or_f df
      end)
  done with | End_of_file -> () end ;
  Scanf.Scanning.close_in ic ;
  (* PRINT *)
  l := List.rev !l ;
  printf "[COMMIT] The following files are waiting for a -commit : \n" ;
  List.iter (printf "\t\t%s\n") !l ;
  l_to_be := !l
  end
(* ==================== *)


(* ===== PRINT CHANGED ===== *)
let print_changed tbl_files =
  if IdMap.is_empty tbl_files
  then printf "[CHANGES] No file changed.\n"
  else begin
    printf 
     "[CHANGES] The following files are different from the version \
      saved and not in the to_be_commited list: \n" ;
    let fct fn stored_key =
      if (Outils.mksha fn <> stored_key) && not (List.mem fn !l_to_be)
      then printf "\t\t%s\n" fn in
    IdMap.iter fct tbl_files
  end
(* ==================== *)


(* ===== MAIN ===== *)
let cmd_status () =
  let repo = Outils.repo_find_chk () in
  Root.real_cwd := Unix.getcwd () ;
  Unix.chdir (Filename.dirname repo) ;
  (* 1) ceux qui attendent *)
  print_to_be repo ;
  printf " ================================================= \n" ;
  (* 2) ceux qui ont changé *)
  let tbl_files = Outils.load_tbl_files repo in
  print_changed tbl_files ;
  printf " =================================================\n" ;
  Unix.chdir !Root.real_cwd 
(* ==================== *)

open Printf
open Root

(* ===== PRINT TO_BE ===== *)
let l_to_be = ref []

let print_cr = printf "\t\x1B[32m-create \x1B[97m%s\n"
let print_rm = printf "\t\x1B[31m-remove \x1B[97m%s\n"
let print_ch = printf "\t\x1B[33m-modify \x1B[97m%s\n"
let print_mv = printf "\t\x1B[33m-move   \x1B[97m%s \x1B[33mto \x1B[97m%s\n"

let print_to_be f_to_cr f_to_ch f_to_rm f_to_mv d_to_cr d_to_mv d_to_rm =
  if f_to_cr=[] && f_to_ch=[] && f_to_rm=[] && f_to_mv=[]
  && d_to_cr=[] && d_to_mv=[] && d_to_rm=[]
  then printf "[COMMIT] There are no files waiting for a -commit.\n"
  else begin
  printf "[COMMIT] The following files are waiting for a -commit : \n" ;
  let lch = fst (List.split f_to_ch)
  and lrm = fst (List.split f_to_rm) in
  let lop,lnp = List.split f_to_mv in
  List.iter print_cr f_to_cr ;
  List.iter print_ch lch ;
  List.iter print_rm lrm ;
  List.iter2 print_mv lop lnp ;
  if d_to_cr<>[] || d_to_mv<>[] || d_to_rm<>[]
  then printf "Some directories will be created,moved or removed.\n" ;
  l_to_be := f_to_cr @ lch @ lrm @ lop 
  end
(* ==================== *)


(* ===== PRINT CHANGED ===== *)
let print_changed tbl_files =
  if IdMap.is_empty tbl_files
  then printf "[CHANGES] Empty branch.\n"
  else begin
    printf 
     "[CHANGES] The following files are different from the version \
      saved and not in the to_be_commited list: \n" ;
    let fct fn stored_key =
      if not (List.mem fn !l_to_be) then 
      if not (Sys.file_exists fn) then print_rm fn
      else if (Outils.mksha fn <> stored_key) then print_ch fn
    in
    IdMap.iter fct tbl_files
  end
(* ==================== *)


(* ===== MAIN ===== *)
let cmd_status () =
  Outils.init () ;
  Root.real_cwd := Unix.getcwd () ;
  Unix.chdir !root ;
  let f_to_cr,f_to_ch,f_to_rm,f_to_mv,d_to_cr,d_to_mv,d_to_rm,tbl_files
    = Pre_commit.compile_to_be () in
  print_to_be repo ;
  printf " ================================================= \n" ;
  print_changed tbl_files ;
  printf " =================================================\n" ;
  Unix.chdir !Root.real_cwd 
(* ==================== *)

open Root
open Scan_diff
open Printf

(* === Ref === *)
let msg = ref ""
let nb_rebuilt = ref 0
let nb_minor = ref 0
let nb_nothing = ref 0
let key_to_suppr = ref []
let nb_lines_add = ref 0
let nb_lines_del = ref 0

(* ===== Fonctions effectives, qui add/remove/modifie/mv etc ===== *)
(* Simple :) *)
let real_d_cr commit_ch d =
  Tree.add_dir d ;
  fprintf commit_ch "CREATE DIR %s\n" d

let real_f_cr commit_ch f =
  Outils.store f !dr_files ;
  nb_lines_add := !nb_lines_add + (Array.length (Outils.readlines f)) ;
  let key = Outils.mksha f in
  Tree.add_file f key ;
  fprintf commit_ch "CREATE FILE %s %s\n" f key ;
  printf "*" ; flush stdout

let start_d_mv commit_ch (op,np) = (*oldpath newpath*)
  Outils.create_dir np ;
  Tree.add_dir np ;
  fprintf commit_ch "MOVE DIR %s %s\n" op np

let start_f_mv commit_ch ((op,key),np) =
  Sys.command (sprintf "cp %s %s" op np) ;
  Tree.add_file np key ;
  fprintf commit_ch "MOVE FILE %s %s %s\n" op np key ;
  printf "*" ; flush stdout

let start_d_rm commit_ch d =
  fprintf commit_ch "REMOVE DIR %s\n" d

let start_f_rm commit_ch (f,key) =
  nb_lines_del := !nb_lines_del + (Array.length (Outils.readlines f)) ;
  fprintf commit_ch "REMOVE FILE %s %s\n" f key ;
  printf "*" ; flush stdout

let final_f_ch key = Outils.remove_hash !dr_files key
let final_d_mv (op,_)     = Tree.remove_d op ;  Outils.remove op
let final_f_mv ((op,_),_) = Tree.remove_f op ;  Outils.remove op
let final_d_rm d          = Tree.remove_d d  ;  Outils.remove d
let final_f_rm f          = Tree.remove_f f  ;  Outils.remove f


(* Compliqué : la fonction de modif, car on sauvegarde les différences *)

let real_f_ch commit_ch (f,old_key) =
  let new_key = Outils.mksha f in
  if old_key <> new_key then begin
  (* Store la nouvelle version *)
    Outils.store f dr_files ;
    Tree.remove_f f ;
    Tree.add_f f new_key ;
  (* Charger l'ancienne, via un fichier où la décompresser *)
    let tmp_old_file = Filename.concat !dr_files "tmp_old_file" in
    let old_ch = open_out tmp_old_file in
    Outils.load old_key !dr_files old_ch ;
    close_out old_ch ;
  (* Utiliser Scan_diff pour calculer les différences *)
    let old_ch = open_in tmp_old_file and new_ch = open_in f in
    let nb_ok,old_in_new,t_placeN = Scan_diff.main old_ch new_ch in
    close_in old_ch ; close_in new_ch ;
    print_debug "Modif de %s, nb = %d, distribution :\n%s\n" f nb_ok
      (Outils.str_list (Array.to_list t_placeN)) ;
    let nb_del,nb_add = Scan_diff.count_diff old_in_new in
    nb_lines_del := nb_del + !nb_lines_del ;
    nb_lines_add := nb_add + !nb_lines_add ;
  (* Charger les deux versions *)
    let t_old = Outils.readlines tmp_old_file in
    let lenO = Array.length t_old in
    let lenN = Array.length t_placeN in
  (* Voir si on garde l'ancien *)
    if (nb_ok * 2 <= lenN) && (lenO > 5)
    then begin 
    (* REBUILT : ON GARDE en mémoire l'old et le new *)
      incr nb_rebuilt ;
      fprintf commit_ch "MODIF REBUILT %s %s %s\n"
        f old_key new_key ;
    end
    else begin
    (* MINOR : On suppr old, et on note dans le fichier de commit les différences *)
    (* On doit noter ce qui appartenait à old. 
       Ce qui appartient à new on l'a toujours *)
      incr nb_minor ;
      fprintf commit_ch "MODIF MINOR %s %s %d\n"
        f new_key (List.length old_in_new) ;
      key_to_suppr := old_key :: !key_to_suppr ;
      let aux_modif = function (* : Scan_diff.change type *)
        | Add(i,j) -> 
            fprintf commit_ch "Add l-%d to l-%d (new numbering)\n" i j
        | Remove(i,j) ->
            fprintf commit_ch "Remove l-%d to l-%d (old numbering) :\n" i j ;
            for l = i to j do
              output_string commit_ch (t_old.(l)^"\n")
            done
        | Modif ((i_old,j_old),(i_new,j_new)) ->
            fprintf commit_ch 
              "Modif from l-%d,l-%d (old numbering) \
               to l-%d,l-%d (new), rm :\n" i_old j_old i_new j_new ;
            for l = i_old to j_old do
              output_string commit_ch (t_old.(l)^"\n")
            done
      in
      List.iter aux_modif old_in_new
    end ;
    Sys.remove tmp_old_file
    end
  else incr nb_nothing ;
  printf "*" ; flush stdout
(* ================ *)


(* ===== cmd_commit ===== *)
let cmd_commit () =
  Outils.init () ;
  Root.real_cwd := Unix.getcwd () ;
  Unix.chdir (Filename.dirname repo) ;
  let f_to_cr,f_to_ch,f_to_rm,f_to_mv,d_to_cr,d_to_mv,d_to_rm,tbl_files
    = Pre_commit.compile_to_be () in
  let nb_f_cr = List.length f_to_cr
  and nb_f_ch = List.length f_to_ch
  and nb_f_rm = List.length f_to_rm
  and nb_f_mv = List.length f_to_mv
  and nb_d_cr = List.length d_to_cr
  and nb_d_rm = List.length d_to_rm
  and nb_d_mv = List.length d_to_mv in
  let nb_f = nb_f_cr + nb_f_ch + nb_f_rm + nb_f_mv
  and nb_d = nb_d_cr + nb_d_rm + nb_d_mv in
  if nb_f+nb_d=0 then eprintf "Nothing to commit, use mg -add first.\n"
  else begin
    printf "-> %d file(s) to handle (and %d directories) :\n|" nb_f nb_d ;
    flush stdout ;
    (* Idée : On écrit et on enregistre toutes les modifications avant
       de faire quoique ce soit d'irréversible. Donc on "register" dans le
       commit qu'on va suppr, avant de le faire effectivement par exemple. *)
    let tmp_file = Filename.concat dr_comms "tmp_commit_file" in
    let commit_ch = open_out tmp_file in

    let pcommit = Outils.find_commit () in
    fprintf commit_ch "\nBranch : %s\n" !branch ;
    fprintf commit_ch "\nParent commits : %s\n" pcommit ;

    List.iter (real_d_cr commit_ch) d_to_cr ;
    List.iter (real_f_cr commit_ch) f_to_cr ;
    List.iter (real_f_ch commit_ch) f_to_ch ;
    List.iter (start_d_mv commit_ch) d_to_mv ;
    List.iter (start_f_mv commit_ch) f_to_mv ;
    List.iter (start_d_rm commit_ch) d_to_rm ;
    List.iter (start_f_rm commit_ch) f_to_rm ;

    fprintf commit_ch "\nMessage :\n%s" !msg ;

    close_out commit_ch ;
    Outils.store tmp_file !dr_comms ;
    let oc = open_out (Filename.concat !dr_brnch !brnch) in
    fprintf oc "last commit : %s\n" (Outils.mksha tmp_file) ;
    close_out oc ;

    List.iter final_f_ch !key_to_suppr ;
    List.iter final_d_mv d_to_mv ;
    List.iter final_f_mv f_to_mv ;
    List.iter final_d_rm d_to_rm ;
    List.iter final_f_rm f_to_rm ;

    if not !bool_print_debug then Sys.remove tmp_file ;
    printf 
    "\nDone. created : %d ; rebuilt : %d ; slightly modified : %d ; \
      unchanged : %d ; removed : %d ; moved : %d\n"
      nb_f_cr !nb_rebuilt !nb_minor !nb_nothing nb_f_rm nb_f_mv
    printf "Total : %d insertions(+), %d deletions(-)\n"
      !nb_lines_del !nb_lines_add ;
  end ;
  Unix.chdir !Root.real_cwd 
(* ================ *)


(* let commit_ch = 
    open_out_gen [Open_creat ; Open_trunc] mkfile_num tmp_file in
   Donne un bug trop bizarre *)

open Root
open Printf

(* 
   Structure d'un fichier de commit de merge, on renvoie 3 commits :
   - L'un destiné à la branche 1, comment passer de 1 à merge, et l'autre
     pour la 2. Ainsi suivre un merge c'est exactement un forward ou un backward
     dans l'une des deux branches. Une fois les deux fichiers de commits produits,
     tout se passe comme si il y avait juste 2 commits simples.
   - Le troisième commit fait la synthèse pour aider à la compréhension
     pour branch_graph. Il synthètise en enregistrant les SHA des commits enjeux.
     Dans les faits il ne contient aucune information indispensable.

   Pour travailler on distingue 4 sous-commits :
  auto_1) 
      Toute les modifs apporté uniquement dans 1 par rapport à l'ancetre. 
      ie 2 et ancetre sont identiques, et 1 différent. Le merge prend alors
      la valeur de 1 qui vraisemblablement est la plus nouvelle.
      Donc auto~1 doit être lu pour passer de 2->merged.
      Remarque si il y a eu la même modif de par et d'autre, on n'a pas à la
      retenir, le but est de construire deux forward de 1/2 vers merged.
  auto_2) Idem servira pour 1->merged. 
  choix~)
      Quand il y avait des modif dans 1 et dans 2 par rapport à
      l'ancêtre au même endroit. 
      -> Ce sont des choix demandés à l'utilisateur au moment du merge
         il faut retenir ces choix.
      -> Mais ils n'auront pas le même impact sur 1 et sur 2.
         D'où choix_1 et choix_2.

 Cas possible : 
  * Un fichier créer ou supprimer -> auto~
  * Au sein d'un fichier, si intersion exactement à la même ligne -> choix~
  * Si suppression de lignes communes (dans 1 rm l1 à l4 et dans 2 rm l3 à l6)
    alors il faut suppr l5 à l6 dans 1 et l1 à l2 dans 2. -> auto~
  * Au sein d'un fichier, deux blocs de *modif* qui se chevauchent -> choix~
  ...

  Remarque : un move ne peut être vu *que* comme un remove + create.
  Donc si un fichier a été déplacé à 2 endroits différents, il sera dupliqué
  dans la version merge. La notion de move n'existe plus.
  On pourrait imaginer chercher la clé. Mais ce n'est pas robuste à l'idée
  d'avoir des fichiers différents au contenu identique.
*)

(* ============================================================= *)
(* Demande à l'user quoi faire si créé 1&2 mais différents *)
let chose_cr_cr (fn,k1,k2) = (* -> bool : vrai si choisi 1 / faux sinon *)
  (* TODO : moins binaire dans le choix, 
     scan les fichiers et demande point par point *)
  printf "==========================\n" ;
  printf "CONFLIT : both create %s: Do you prefer OPTION 1 :\n" fn ;
  printf "==========================\n" ;
  Outils.load k1 !dr_files stdout ;
  printf "==========================\n" ;
  printf "or OPTION 2 :\n" ;
  Outils.load k2 !dr_files stdout ;
  printf "==========================\n" ;
  printf " \"1\" or \"2\" ?\n" ;
  let rep = ref "" in
  while !rep<>"2" && !rep<>"1" do rep := read_line () done ;
  !rep = "1" (*:bool*)

(* Demande si l'un a suppr et l'autre à modifié *)
let chose_rm_ch br_rm br_ch (fn,k) =
  printf "==========================\n" ;
  printf "CONFLIT : %s has been deleted in branch %s \
    and modified in branch %s :\n" fn br_rm br_ch ;
  printf "==========================\n" ;
  Outils.load k !dr_files stdout ;
  printf "==========================\n" ;
  printf " \"1\" to remove or \"2\" to change ?\n" ;
  let rep = ref "" in
  while !rep<>"2" && !rep<>"1" do rep := read_line () done ;
  !rep = "1" (*:bool*)


(* ============================================================= *)
(* Départage les modifications d'un même fichier *)
let scan_double_ch (fn,_,k1,k2)(*fn,ka,k1,k2)*) = (* TODO *)
  eprintf 
    "Désolé je ne gère pas encore les conflits avec des\n\
     modifications différentes d'un même fichier (ici %s) dans les deux\n\
     branches à défaut je garde la version de la branche 1\n" fn ;
  (fn,k2,k1)



(* ============================================================= *)
(* La fonction qui fait le boulot : - équivalent de pre_commit.ml *)
let scan_merge br1 br2 br_anc = 
  (* ETAPE 0 : Énumération *)
  let st_d1 ,tbl_f1 = Tree.enumerate_all br1
  and st_d2 ,tbl_f2 = Tree.enumerate_all br2
  and st_da ,tbl_fa = Tree.enumerate_all br_anc in
 
  (* ETAPE 1 : Repérer les changements *)
  let l_d_1cr_2x  = ref [] and l_d_1x_2cr  = ref [] 
  and l_d_1rm_2x  = ref [] and l_d_1x_2rm  = ref []
  and l_f_1cr_2x  = ref [] and l_f_1x_2cr  = ref [] 
  and l_f_1rm_2x  = ref [] and l_f_1x_2rm  = ref [] 
  and l_f_1ch_2x  = ref [] and l_f_1x_2ch  = ref [] 
  and l_f_1rm_2ch = ref [] and l_f_1ch_2rm = ref [] 
  and l_f_1ch_2ch = ref [] and l_f_1cr_2cr = ref [] in

(* Pour controler récupérer la liste des dir inchangés, on utilise IdSet.inter ;
   mais on peut aussi l'utiliser pour les fichiers. En effet ce sont des st
   de paires (nom,key), donc par l'inter on controle l'égalité des clés. *)

  let all_dirs = IdSet.union st_d1 (IdSet.union st_d2 st_da) in
  let all_files = 
    IdSet.union
    (tbl_fa |> IdMap.bindings |> List.split |> fst |> Outils.set_of_list)
    (IdSet.union 
    (tbl_f1 |> IdMap.bindings |> List.split |> fst |> Outils.set_of_list)
    (tbl_f2 |> IdMap.bindings |> List.split |> fst |> Outils.set_of_list))
    |> IdSet.elements 
  in

  print_debug 
    "=>Dossiers dans 1 : \n%s\n=>dossiers dans \
     2 : \n%s\n=>dossier dans l'anc : \n%s\n"
    (String.concat "\n" (IdSet.elements st_d1))
    (String.concat "\n" (IdSet.elements st_d2))
    (String.concat "\n" (IdSet.elements st_da)) ;
  print_debug "All_files concerned by the merge : \
    \n%s\n" (String.concat "\n" all_files) ;


  IdSet.iter
  (fun d -> 
    match (IdSet.mem d st_da),
          (IdSet.mem d st_d1),
          (IdSet.mem d st_d2) with
  | true  , true  , false -> Outils.append l_d_1x_2rm d
  | true  , false , true  -> Outils.append l_d_1rm_2x d
  | false , false , true  -> Outils.append l_d_1x_2cr d
  | false , true  , false -> Outils.append l_d_1cr_2x d
  | _ , _ , _ -> ()
  ) all_dirs ;


  List.iter
  (fun fn -> 
    match (IdMap.find_opt fn tbl_fa),
          (IdMap.find_opt fn tbl_f1),
          (IdMap.find_opt fn tbl_f2) with
  | Some ka , Some k1 , None    when ka=k1  -> Outils.append l_f_1x_2rm  (fn,k1)
  | Some _  , Some k1 , None                -> Outils.append l_f_1ch_2rm (fn,k1) (*=*)
  | Some ka , None    , Some k2 when ka=k2  -> Outils.append l_f_1rm_2x  (fn,k2)
  | Some _  , None    , Some k2             -> Outils.append l_f_1rm_2ch (fn,k2) (*=*)
  | None    , Some k1 , None                -> Outils.append l_f_1cr_2x  (fn,k1)
  | None    , None    , Some k2             -> Outils.append l_f_1x_2cr  (fn,k2)
  | None    , Some k1 , Some k2 when k1<>k2 -> Outils.append l_f_1cr_2cr (fn,k1,k2) (*=*)
  | Some ka , Some k1 , Some k2 when k1=ka && ka<>k2 -> Outils.append l_f_1x_2ch (fn,ka,k2)
  | Some ka , Some k1 , Some k2 when k2=ka && ka<>k1 -> Outils.append l_f_1ch_2x (fn,ka,k1)
  | Some ka , Some k1 , Some k2 when k1<>ka && k2<>ka && k1<>k2 
    -> Outils.append l_f_1ch_2ch (fn,ka,k1,k2) (*=*)
  | _ , _ , _ -> ()
  ) all_files ;


  (* ETAPE 2 : Résolution des conflits *)
  let l_chose_cr1,l_chose_cr2 = List.partition chose_cr_cr !l_f_1cr_2cr in
  Outils.extend l_f_1cr_2x (List.map (fun (fn,k1,_) -> (fn,k1)) l_chose_cr1) ;
  Outils.extend l_f_1rm_2x (List.map (fun (fn,_,k2) -> (fn,k2)) l_chose_cr1) ;
  Outils.extend l_f_1x_2cr (List.map (fun (fn,_,k2) -> (fn,k2)) l_chose_cr2) ;
  Outils.extend l_f_1x_2rm (List.map (fun (fn,k1,_) -> (fn,k1)) l_chose_cr2) ;

  let l_chose_rm1,l_chose_ch2 = List.partition (chose_rm_ch br1 br2) !l_f_1rm_2ch in 
  let l_chose_rm2,l_chose_ch1 = List.partition (chose_rm_ch br2 br1) !l_f_1ch_2rm in 
  Outils.extend l_f_1rm_2x l_chose_rm1 ;
  Outils.extend l_f_1x_2cr l_chose_ch2 ;
  Outils.extend l_f_1x_2rm l_chose_rm2 ;
  Outils.extend l_f_1cr_2x l_chose_ch1 ;

  (* TODO *)
  Outils.extend l_f_1ch_2x (List.map scan_double_ch !l_f_1ch_2ch) ;

  (* RETURN, il n'y a plus de conflits : *) 
  !l_d_1cr_2x , !l_d_1x_2cr ,
  !l_d_1rm_2x , !l_d_1x_2rm ,
  !l_f_1cr_2x , !l_f_1x_2cr ,
  !l_f_1rm_2x , !l_f_1x_2rm ,
  !l_f_1ch_2x , !l_f_1x_2ch 
  
(* ============================================================= *)

(* La fonction qui print les 3 commits *)
let write_merge br1 cm1 br2 cm2 br_anc = 
  let l_d_1cr_2x , l_d_1x_2cr ,
      l_d_1rm_2x , l_d_1x_2rm ,
      l_f_1cr_2x , l_f_1x_2cr ,
      l_f_1rm_2x , l_f_1x_2rm ,
      l_f_1ch_2x , l_f_1x_2ch = scan_merge br1 br2 br_anc in

  let msg1 = sprintf "Resulting 1 of the merge of %s and %s.\n" cm1 cm2 in
  let sha1 =
  Commit.write_commit msg1 cm1
    l_d_1x_2cr [] l_d_1x_2rm
    [] [] [] l_f_1x_2rm
    l_f_1x_2cr l_f_1x_2ch
  in

  let msg2 = sprintf "Resulting 2 of the merge of %s and %s.\n" cm1 cm2 in
  let sha2 =
  Commit.write_commit msg2 cm2
    l_d_1cr_2x [] l_d_1rm_2x
    [] [] [] l_f_1rm_2x
    l_f_1cr_2x l_f_1ch_2x
  in

  (* On lie les deux commits ensemble, et on fait le commit merge *)
  let sha1_m = Outils.sha_name (sha1 ^ sha2) 
  and sha2_m = Outils.sha_name (sha2 ^ sha1) in
  Outils.rename_commit sha1 sha1_m ;
  Outils.rename_commit sha2 sha2_m ;
  let tmp_file = Filename.concat !dr_comms "tmp_commit_merge_file" in
  let cch = open_out tmp_file in
  fprintf cch "MERGE\nResulting commits : %s and %s\n" sha1_m sha2_m ;
  close_out cch ;
  Outils.store tmp_file !dr_comms ;
  let sha = Outils.mksha tmp_file in
  if not !bool_print_debug then Sys.remove tmp_file ;
  ( sha1_m , sha2_m , sha )
    
(* ============================================================= *)
  
  
(* ============================================================= *)

let cmd_merge l =
  (* ETAPE 0 : Initialisation *)
  Outils.init () ;
  Outils.rootwd () ;
  let br1,br2,br_merge = match l with
  | [br1;br2] -> br1,br2,br1
  | [br1;br2;br_merge] -> br1,br2,br_merge
  | _ -> eprintf "Syntax error on the merge cmd, use : \n\
    <mg -branch -merge \"branch_name 1\" \"branch_name 2\"\
    [\"post_merge_branch_name\" (default branch 1)]>\n" ;
    exit 1
  in
  let cm1 = Outils.find_commit br1 
  and cm2 = Outils.find_commit br2 in


  (* ETAPE 1 : retrouver le plus proche ancêtre commun et cmt y accéder *)
  let commits = Outils.list_sha !dr_comms in
  let gup,_,_ = Branch.make_commit_graph commits in
  let tbl = ref (IdMap.empty) in (* tbl des dist à br1 *)

  let qu = Queue.create () in
  Queue.add (cm1,0,[]) qu ;
  while not (Queue.is_empty qu) do
    let (cm,p,chemin) = Queue.pop qu in
    if cm=cm2 then 
    ( eprintf "Branch %s is on a parent state of branch %s\n"  br1 br2 ; exit 0) ;
    if not (IdMap.mem cm !tbl) then begin
      tbl := IdMap.add cm (p,chemin) !tbl ;
      if cm<>"none" then 
      IdSet.iter 
        (fun pcm -> Queue.add (pcm,p+1,cm::chemin) qu) 
        (IdMap.find cm gup)
  end done ;

  let cm_anc = ref "" and p_min_anc = ref 0
  and br_pp  = ref "" and p_max_anc = ref max_int 
  and chemin_acc = ref [] in

  let tbl2 = ref (IdSet.empty) in
  Queue.add (cm2,0,[]) qu ;
  while not (Queue.is_empty qu) do
    let (cm,p2,chemin2) = Queue.pop qu in
    if cm=cm1 then 
    ( eprintf "Branch %s is on a parent state of branch %s\n"  br2 br1 ; exit 0) ;
    if not (IdSet.mem cm !tbl2) then begin
      tbl2 := IdSet.add cm !tbl2 ;
      if cm<>"none" then 
      IdSet.iter 
        (fun pcm -> Queue.add (pcm,p2+1,cm::chemin2) qu) 
        (IdMap.find cm gup) ;
      match IdMap.find_opt cm !tbl with
      | None -> ()
      | Some (p1,chemin1) -> 
          let p_min = min p1 p2
          and p_max = max p1 p2 in
          if p_max < !p_max_anc then begin
            p_max_anc := p_max ;
            p_min_anc := p_min ;
            cm_anc := cm ;
            if p1 = p_min 
            then ( br_pp := br1 ; chemin_acc := chemin1 )
            else ( br_pp := br2 ; chemin_acc := chemin2 ) 
          end 
    end
  done ;

  chemin_acc := List.rev !chemin_acc ;
  print_debug 
    "Commit ancetre : %s via la branche %s en %d (sinon %d), chemin : \n%s\n"
    !cm_anc !br_pp !p_min_anc !p_max_anc (String.concat "\n->" !chemin_acc) ;


  (* ETAPE 2 : Créer la branche temporaire, merge, puis la supprime *)
  let br_tmp = "tmp_for_merge" in
  Branch.create !br_pp br_tmp ;
  List.iter (Branch_mvt.backward br_tmp) !chemin_acc ;
  let (sha1,sha2,sha) = write_merge br1 cm1 br2 cm2 br_tmp in
  Branch_mvt.forward br1 sha1 ; (* vrai travail *)
  Branch_mvt.forward br2 sha2 ;
  Branch_mvt.forward br1 sha ; (* pour clarifier le graphe *)
  Branch_mvt.forward br2 sha ;
  Branch.delete br_tmp ;
  if br_merge=br1 then 
  ( Branch.delete br2 ;
    if !branch = br2 then Outils.branch_switch br1 )
  else if br_merge=br2 then
  ( Branch.delete br1 ;
    if !branch = br1 then Outils.branch_switch br2 )
  else
  ( Branch.create br1 br_merge ;
    Branch.delete br1 ;
    Branch.delete br2 ;
    if !branch = br1 || !branch = br2 
    then Outils.branch_switch br_merge ) ;
  Outils.realwd ()
  



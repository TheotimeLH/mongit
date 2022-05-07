let numerote_old old_ch =
  let nb_num = ref 0 in
  let tbl_num = Hashtbl.create 20 in
  let lines = ref [] in
  begin try while true do
    let l = input_line old_ch in
    match Hashtbl.find_opt tbl_num l with
    | None ->
        Hashtbl.add tbl_num l !nb_num ;
        lines := !nb_num :: !lines ;
        incr nb_num
    | Some num ->
        lines := num :: !lines
  done
  with | End_of_file -> () end ;
  !nb_num , tbl_num , Array.of_list (List.rev !lines)


let indice_new new_ch nb_num tbl_num =
  let t_ind = Array.make nb_num [] in
  let lines = ref [] in
  let i = ref (-1) in
  begin try while true do
    let l = input_line new_ch in
    incr i ;
    match Hashtbl.find_opt tbl_num l with
    | Some num -> 
        t_ind.(num) <- !i :: t_ind.(num) ;
        lines := num :: !lines
    | None -> 
        lines := (-1) :: !lines
  done
  with | End_of_file -> () end ;
  ( Array.map List.rev t_ind ,
    Array.of_list (List.rev !lines) )


let eval_sol t_placeN =
  let lenN = Array.length t_placeN in
  let max_acc = ref (-1) in
  let sum = ref 0 in
  for i = 1 to lenN - 1 do
    if (t_placeN.(i) > !max_acc)
    && (t_placeN.(i) - 1 = t_placeN.(i-1))
    then (max_acc := t_placeN.(i) ; incr sum)
  done ;
  !sum

exception Fin

type change = 
  | Modif of (int * int) * (int * int)
  | Add of (int * int)
  | Remove of (int * int)

let mk_sol_finale t_placeN lenO =
  let lenN = Array.length t_placeN in
  (* On commence par clean la sol, conjecture : la sol est déjà clean *)
  (* Les morceaux de taille 1 *)
  if (lenN > 1) then 
  ( if (t_placeN.(0) + 1 <> t_placeN.(1)) 
      then t_placeN.(0) <- -1 ;
    if (t_placeN.(lenN - 2) + 1 <> t_placeN.(lenN - 1)) 
      then t_placeN.(lenN - 1) <- -1 
  ) ;
  for i = 1 to lenN - 2 do
    if (t_placeN.(i-1)+1<>t_placeN.(i)) && (t_placeN.(i+1)-1<>t_placeN.(i))
    then t_placeN.(i) <- -1
  done ;
  (* Puis les décroissances *)
  let max_acc = ref (-1) in
  for i = 0 to lenN - 1 do
    if t_placeN.(i) <= !max_acc
    then t_placeN.(i) <- -1
    else max_acc := t_placeN.(i)
  done ;
  (* Fin clean *)
  (* Compte les points conservés *)
  let sum = ref 0 in
  Array.iter (fun x -> if x<> -1 then incr sum) t_placeN ;
  (* *)

  let sol = ref [] in (* change list *)
  let rec next i =
    if i >= lenN then raise Fin ;
    if t_placeN.(i) <> -1 then i
    else next (i+1)
  in
  let i_new = ref 0 
  and i_old = ref 0 in
  begin
  try while !i_new < lenN do
    let i_new_next = next !i_new in
    let i_old_next = t_placeN.(i_new_next) in
    let b_new = !i_new <> i_new_next
    and b_old = !i_old <> i_old_next in
    if b_new || b_old then 
      sol := 
      ( match b_new , b_old with
        | true , false -> Add (!i_new , i_new_next -1)
        | false , true -> Remove (!i_old , i_old_next -1)
        | true , true -> Modif ((!i_old , i_old_next -1),(!i_new , i_new_next -1))
        | false , false -> failwith "z" )
      :: !sol ;
    i_new := i_new_next + 1 ;
    i_old := i_old_next + 1
  done 
  with | Fin ->
    let b_new = !i_new < lenN 
    and b_old = !i_old < lenO in
    if b_new || b_old then 
      sol := 
      ( match b_new , b_old with
        | true , false -> Add (!i_new , lenN-1)
        | false , true -> Remove (!i_old , lenO-1)
        | true , true -> Modif ((!i_old , lenO-1),(!i_new , lenN-1))
        | false , false -> failwith "z" )
      :: !sol
  end ;
  !sum,!sol,t_placeN
    




(* Personne ne retourne en arrière, on prend le premier slot
   et on ne pourra que avancer. *)
let main old_ch new_ch =
  (* Les deux channels ouverts en lecture seule *)
  let nb_num , tbl_num , t_numO = numerote_old old_ch in
  let t_indN , t_numN = indice_new new_ch nb_num tbl_num in
  
  let lenO = Array.length t_numO
  and lenN = Array.length t_numN in 
  let t_placeN = Array.make lenN (-1) in
  let t_placeO = Array.make lenO (-1) in

  (* Etape 1 : tous les lancer *)
  let t_indN' = Array.copy t_indN in
  (* let tbl_dispo = Array.make nb_num [] in *)
  for i_old = 0 to lenO -1 do
    let num = t_numO.(i_old) in
    match t_indN'.(num) with
    | [] -> () 
        (* tbl_dispo.(num) <- i_old :: tbl_dispo.(num) *)
    | next :: q -> 
        t_placeN.(next) <- i_old ;
        t_placeO.(i_old) <- next ;
        t_indN'.(num) <- q
  done ;

  (* Etape 2 : compresser, quitte à pousser *)
  let relance i_new i_old t_placeN t_placeO =
    t_placeO.(i_old) <- -1 ;
    let num = t_numO.(i_old) in
    let i_old = ref i_old in
    List.exists 
     (fun i -> 
        if i < i_new then false
        else if t_placeN.(i) <> -1 then (* On le chasse *)
          ( let i_old' = t_placeN.(i) in
            t_placeO.(i_old') <- -1 ;
            t_placeN.(i) <- !i_old ;
            t_placeO.(!i_old) <- i ;
            i_old := i_old' ;
            false )
        else 
          ( t_placeN.(i) <- !i_old ; 
            t_placeO.(!i_old) <- i ;
            true )
     ) t_indN.(num)
    |> ignore
    (* if not b then tbl_dispo.(num) <- !i_old :: tbl_dispo.(num) *)
  in

  let compress t_placeN t_placeO =
    for i_new = lenN-1 downto 1 do
      let i_old = t_placeN.(i_new) in
      let i_prec = t_placeN.(i_new - 1) in
      if (i_old > 0) && (i_prec <> i_old - 1)
      && (t_numN.(i_new -1) = t_numO.(i_old -1)) 
      then begin
        if i_prec <> -1 then relance i_new i_prec t_placeN t_placeO ;
        t_placeN.(t_placeO.(i_old - 1)) <- -1 ;
        t_placeO.(i_old -1) <- i_new -1 ;
        t_placeN.(i_new -1) <- i_old -1 ;
      end
    done
  in
  compress t_placeN t_placeO ;

  let debut = ref 0 in
  let serie = ref false in
  let sol_act = ref (eval_sol t_placeN) in
  let t_placeN = ref t_placeN
  and t_placeO = ref t_placeO in
  for i_new = 1 to lenN -1 do
    if !t_placeN.(i_new) -1 = !t_placeN.(i_new -1) 
    then (if not !serie then (serie := true ; debut := i_new - 1))
    else if !serie then begin
      let t_placeN_save = Array.copy !t_placeN 
      and t_placeO_save = Array.copy !t_placeO in
      for i = !debut to i_new -1 do
        let i_old = !t_placeN.(i) in
        !t_placeN.(i) <- -1 ;
        relance i_new i_old !t_placeN !t_placeO
      done ;
      let sol_else = eval_sol !t_placeN in
      if !sol_act > sol_else 
      then (t_placeN := t_placeN_save ; t_placeO := t_placeO_save)
      else sol_act := sol_else ;
      serie := false
    end
  done ;

  compress !t_placeN !t_placeO ;

  mk_sol_finale !t_placeN lenO
(* Pour tester :
  print_endline (String.concat " " 
    (Array.to_list (Array.map string_of_int !t_placeN)))

let () =
  let oldch = open_in "old.ml"
  and newch = open_in "new.ml" in
  main oldch newch ;
  close_in oldch ; close_in newch *)

(* === AUX === *)
let count_diff (sol : change list) =
  let nb_add = ref 0 in
  let nb_del = ref 0 in
  let fct = function
    | Add    (i_new,j_new) -> 
        nb_add := j_new - i_new + 1 + !nb_add
    | Remove (i_old,j_old) -> 
        nb_del := j_old - i_old + 1 + !nb_del
    | Modif ((i_old,j_old),(i_new,j_new)) ->
        nb_add := j_new - i_new + 1 + !nb_add ;
        nb_del := j_old - i_old + 1 + !nb_del
  in
  List.iter fct sol ;
  !nb_del,!nb_add

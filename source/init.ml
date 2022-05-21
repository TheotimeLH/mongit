open Printf

let () =
  (* PATH TO SOURCES *)
  let oc = open_out "mgrc.ml" in
  fprintf oc "let path_sources = \"%s\"" (Unix.getcwd ()) ;
  printf "Source directory := %s\n" (Unix.getcwd ()) ;

  (* PATH TO BIN *)
  let lines = Outils.readlines "Makefile" in
  Scanf.sscanf lines.(0) "path_bin_file=%s" 
  (fun p_bin -> 
    if p_bin = "/tmp" then begin
      printf 
      "mg is a version manager strongly inspired by Git. To use it comfortably\n\
       I propose to place the bin file in a dir accessible with the PATH.\n\
       Where do you want the bin file to be ?\n" ;
      let rep = read_line () in
      if not (Sys.file_exists rep) 
      then printf 
        "%s doesn't exists, the bin will stay in the source dir.\n" rep
      else begin
      let oc = open_out "Makefile" in
      fprintf oc "path_bin_file = %s\n" rep ;
      for i = 1 to Array.length lines - 1 do
        fprintf oc "%s\n" lines.(i)
      done ;
      close_out oc ;
      let env_path = Unix.getenv "PATH" in
      let l_dir = String.split_on_char ':' env_path in
      if not (List.mem rep l_dir)
      then printf 
        "You gave a dir which isn't in the PATH, maybe you should \n\
        consider adding the line below to your ~/.bashrc\n\
        export PATH=\"%s:$PATH\"\n" rep
      else printf "Done.\n"
      end
    end
  )

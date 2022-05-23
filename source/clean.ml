open Printf

let () =
  (* PATH TO SOURCES *)
  let oc = open_out "mgrc.ml" in
  fprintf oc "let path_sources = \"none\"" ;
  close_out oc ;

  (* PATH TO BIN *)
  let lines = Outils.readlines "Makefile" in
  let oc = open_out "Makefile" in
  fprintf oc "path_bin_file=/tmp\n" ;
  for i = 1 to Array.length lines - 1 do
    fprintf oc "%s\n" lines.(i)
  done 


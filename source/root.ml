open Printf

let bool_print_debug = ref false

let print_debug str =
  if !bool_print_debug then fprintf stdout str
  else ifprintf stdout str

exception No_repo
exception Not_exists

let mkdir_num = 509

type obj = 
  | File of string

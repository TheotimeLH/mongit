open Printf

let bool_print_debug = ref false
let bool_print_detail = ref false
let include_secret = ref false
let not_real = ref false

let print_debug str =
  if !bool_print_debug then fprintf stdout str
  else ifprintf stdout str

let print_detail str =
  if !bool_print_detail then fprintf stdout str
  else ifprintf stdout str

exception No_repo
exception Not_exists
exception Not_in_the_tree

module IdMap = Map.Make(String)

let mkdir_num = 0o775
let mkfile_num = 0o664

let real_cwd = ref "" 
let repo = ref ""
let root = ref ""
let branch = ref ""
let to_be = ref ""
let dr_trees = ref ""
let dr_comms = ref ""
let dr_files = ref ""
let dr_brnch = ref ""

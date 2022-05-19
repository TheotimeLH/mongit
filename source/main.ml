open Arg
open Printf

let subparser = ref "default"
let speclist = ref []

(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)
(* ~~~~~ COMMIT PARSER ~~~~~ *)
let subparser_commit () =
  subparser := "commit" ;
  speclist := [
   ("-m",
      Set_string Commit.msg ,
      "add a message to the commit");
  ]
(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)



(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)
(* ~~~~~ BRANCH PARSER ~~~~~ *)
let subparser_branch () =
  subparser := "branch" ;
  speclist := [
   ("-create",
      String Branch.cmd_create ,
      "create a new branch on place");
   ("-list",
      Unit Branch.cmd_list ,
      "list all existing branches");
   ("-switch",
      String Branch.cmd_switch ,
      "change of current/HEAD branch");
   ("-graph",
      Unit Branch.cmd_graph ,
      "display a graph of the commits and branches");
   ("-forward",
      Int Branch.cmd_forward ,
      "Move forward the current branch a number of steps");
   ("-backward",
      Int Branch.cmd_backward ,
      "Move backward the current branch a number of steps");
   ("-merge",
      Rest_all Branch_merge.cmd_merge,
      "Merge two branches, using their closest common ancestor as ref.");
  ]
(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)



  
(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)
(* ~~~~~ DEFAULT PARSER ~~~~~ *)
let () = speclist :=
(* ===== INIT ===== *)
  [("-init",
      Unit Basic_cmds.init , 
      "create a new repository for the cwd\n");
(* ================================== *)

(* ===== BASIC COMMIT COMMANDS : ADD... COMMIT ===== *)
   ("-add",
      Rest (Pre_commit.pre_commit_cmd "add"),
      "set the file/dir as to be saved/commited");
   ("-minus",
      Rest (Pre_commit.pre_commit_cmd "minus"),
      "precise a -add, by withdrawing file/dir \
       from \"to_be_commited\".");
   ("-move",
      Rest_all Pre_commit.cmd_move,
      "move the file/dir in the current.\
       Check -only_on_repo.");
   ("-remove",
      Rest (Pre_commit.pre_commit_cmd "remove"),
      "remove the file/dir in the current branch.\
       Check -only_on_repo.");
   ("-commit",
      Unit subparser_commit,
      "save the modifications\n");
(* ================================== *)

(* ===== VERSION CONTROL ===== *)
   ("-ls",
      Unit Tree.cmd_ls , 
      "display a graph of the current copy");
   ("-status",
      Unit Status.cmd_status , 
      "show the status of the current branch");
   ("-restore",
      String Restore.cmd_restore ,
      "restore a file/dir\n");
(* ================================== *)

(* ===== ACCES TO BRANCH SUBPARSER ===== *)
   ("-branch",
      Unit subparser_branch,
      "to acces commands on branches\n");
(* ================================== *)

(* ===== OPTIONS ===== *)
   ("-only_on_repo", 
      Set Root.only_on_repo , 
      "limit rm and mv cmds so that \
       they only affect the repo");
   ("-include_secret", 
      Set Root.include_secret , 
      "enable operations over secret files");
   ("-detail", 
      Set Root.bool_print_detail , 
      "Detail more what happens");
   ("-debug", 
      Set Root.bool_print_debug , 
      "enable debug messages\n");
(* ================================== *)

(* ===== SIMPLE CMDS : CAT/LIST COMMIT/FILE ===== *)
   ("-reset_commit",
      Unit Basic_cmds.cmd_reset_commit,
      "empty file to_be_commited.");
   ("-cat_file",
      String Basic_cmds.cat_file ,
      "print a file stored given a sha");
   ("-cat_commit",
      String Basic_cmds.cat_commit ,
      "print the content of a commit given a sha");
   ("-list_commits",
      Unit Basic_cmds.cmd_list_commits ,
      "list existing sha commits");
   ("-list_files",
      Unit Basic_cmds.cmd_list_files ,
      "list stored sha files/versions\n");
(* ================================== *)

(* ===== REMOVE ===== *)
   ("-remove_repo",
      Unit Basic_cmds.remove_repo ,
      "Remove the cwd repo.");
(* ================================== *)

(* ===== VERY SPECIAL COMMAND TO UPDATE MG ===== *)
   ("-update",
      Unit Basic_cmds.update ,
      "Very special command to update mg.")
(* ================================== *)
   ]
(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)


let fct_anon s =
  if s<>"" then printf "Unparsed word : %s\n" s

let () = Arg.parse_dynamic speclist fct_anon ""
let () = match !subparser with
  | "commit" -> Commit.cmd_commit ()
  | _ -> ()

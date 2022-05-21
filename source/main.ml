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
      "adds a message to the commit");
  ]
(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)



(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)
(* ~~~~~ BRANCH PARSER ~~~~~ *)
let subparser_branch () =
  subparser := "branch" ;
  speclist := [
   ("-create",
      String Branch.cmd_create ,
      "creates a new branch on place");
   ("-list",
      Unit Branch.cmd_list ,
      "lists existing branches");
   ("-switch",
      String Branch.cmd_switch ,
      "changes current/HEAD branch");
   ("-graph",
      Unit Branch.cmd_graph ,
      "displays a graph of the commits and branches");
   ("-forward",
      Int Branch.cmd_forward ,
      "moves forward the current branch a number of steps");
   ("-backward",
      Int Branch.cmd_backward ,
      "moves backward the current branch a number of steps");
   ("-merge",
      Rest_all Branch_merge.cmd_merge,
      "merges two branches, using their closest common ancestor as ref.");
  ]
(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)



  
(* =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= *)
(* ~~~~~ DEFAULT PARSER ~~~~~ *)
let () = speclist :=
(* ===== INIT ===== *)
  [("-init",
      Unit Basic_cmds.init , 
      "creates a new repository for the cwd\n");
(* ================================== *)

(* ===== BASIC COMMIT COMMANDS : ADD... COMMIT ===== *)
   ("-add",
      Rest (Pre_commit.pre_commit_cmd "add"),
      "sets the file/dir as to be saved/commited");
   ("-minus",
      Rest (Pre_commit.pre_commit_cmd "minus"),
      "precises a -add, by withdrawing file/dir \
       from \"to_be_commited\".");
   ("-move",
      Rest_all Pre_commit.cmd_move,
      "moves the file/dir in the current branch. \
       Check -only_on_repo.");
   ("-remove",
      Rest (Pre_commit.pre_commit_cmd "remove"),
      "removes the file/dir in the current branch. \
       Check -only_on_repo.");
   ("-commit",
      Unit subparser_commit,
      "saves the modifications\n");
(* ================================== *)

(* ===== VERSION CONTROL ===== *)
   ("-ls",
      Unit Tree.cmd_ls , 
      "displays a graph of the current copy");
   ("-status",
      Unit Status.cmd_status , 
      "shows the status of the current branch");
   ("-restore",
      String Restore.cmd_restore ,
      "restores a file/dir\n");
(* ================================== *)

(* ===== ACCES TO BRANCH SUBPARSER ===== *)
   ("-branch",
      Unit subparser_branch,
      "to acces commands on branches\n");
(* ================================== *)

(* ===== OPTIONS ===== *)
   ("-only_on_repo", 
      Set Root.only_on_repo , 
      "limits rm and mv cmds so that \
       they only affect the repo");
   ("-include_secret", 
      Set Root.include_secret , 
      "enables operations over secret files");
   ("-detail", 
      Set Root.bool_print_detail , 
      "enables detail messages, to help user");
   ("-debug", 
      Set Root.bool_print_debug , 
      "enables debug messages, to help dev\n");
(* ================================== *)

(* ===== SIMPLE CMDS : CAT/LIST COMMIT/FILE ===== *)
   ("-reset_commit",
      Unit Basic_cmds.cmd_reset_commit,
      "empties the \"to_be_commited\" list");
   ("-cat_file",
      String Basic_cmds.cat_file ,
      "prints a file stored given a sha");
   ("-cat_commit",
      String Basic_cmds.cat_commit ,
      "prints the content of a commit given a sha");
   ("-list_commits",
      Unit Basic_cmds.cmd_list_commits ,
      "lists existing sha commits");
   ("-list_files",
      Unit Basic_cmds.cmd_list_files ,
      "lists stored sha files/versions\n");
(* ================================== *)

(* ===== REMOVE ===== *)
   ("-remove_repo",
      Unit Basic_cmds.remove_repo ,
      "removes the cwd repo.");
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

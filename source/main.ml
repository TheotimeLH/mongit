open Arg

let subparser = ref "default"
let speclist = ref []

let subparser_commit () =
  subparser := "commit" ;
  speclist := [
   ("-m",
      Set_string Commit.msg ,
      "add a message to the commit");
  ]

let subparser_branch () =
  subparser := "branch" ;
  speclist := [
   ("-create",
      String Branch.cmd_create ,
      "create a new branch on place");
   ("-list",
      Unit Branch.cmd_list ,
      "list all the existing branches");
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
      "Merge two branches, using their closest common ancestor as reference.");
  ]

let () = speclist :=
  [("-debug", 
      Set Root.bool_print_debug , 
      "Print debug messages");
   ("-detail", 
      Set Root.bool_print_detail , 
      "Detail more what has been done");
   ("-include_secret", 
      Set Root.include_secret , 
      "Enables operations over secret files");
   ("-init",
      Unit Basic_cmds.init , 
      "Create a new repository for the current working directory.");
   ("-remove_repo",
      Unit Basic_cmds.remove_repo ,
      "Remove the cwd repo.");
   ("-update",
      Unit Basic_cmds.update ,
      "Update mongit.");
   ("-hash_file",
      String Basic_cmds.hash_file ,
      "< mg -hash_file \"filename\" > will store \"filename\" at .mongit/files/");
   ("-cat_file",
      String Basic_cmds.cat_file ,
      "< mg -cat_file \"sha_key\" > will print out the file .mongit/files/sha_key");
   ("-cat_commit",
      String Basic_cmds.cat_commit ,
      "< mg -cat_file \"sha_key\" > will print out the file .mongit/commits/sha_key");
   ("-add",
      Rest (Pre_commit.pre_commit_cmd "add"),
      "< mg -add \"filename\" > set the file/directory as to be saved on the next commit");
   ("-minus",
      Rest (Pre_commit.pre_commit_cmd "minus"),
      "< mg -minus \"filename\" > remove the file/dir from the list of files to be commited. \n\
       Can be used to undo a -add, or to be more precise. \n\
       For instance you can -add a whole directory and then -minus just one file the dir contains.");
   ("-remove",
      Rest (Pre_commit.pre_commit_cmd "remove"),
      "Remove the file/dir from the current copy branch.");
   ("-move",
      Rest_all Pre_commit.cmd_move,
      "Move the file/dir both in the real working directory and in the current copy branch.");
   ("-commit",
      Unit subparser_commit,
      "Save the modifications in the repo.");
   ("-reset_commit",
      Unit Basic_cmds.cmd_reset_commit,
      "Empty file to_be_commited.");
   ("-ls",
      Unit Tree.cmd_ls , 
      "Create a \"repo_tree.pdf\" file (using Graphviz's dot command).");
   ("-status",
      Unit Status.cmd_status , 
      "Show the status of the copy.");
   ("-restore",
      String Restore.cmd_restore ,
      "Restore the file requested (or the whole directory).");
   ("-only_on_repo", 
      Set Root.only_on_repo , 
      "Limits remove and move commands so that they only affect the repo");
   ("-branch",
      Unit subparser_branch,
      "To acces commands on branches.");
   ]


let () = Arg.parse_dynamic speclist Tmp.aff_qlqch ""

let () = match !subparser with
  | "commit" -> Commit.cmd_commit ()
  | _ -> ()

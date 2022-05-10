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
let () = speclist :=
  [("-debug", 
      Set Root.bool_print_debug , 
      "Print debug messages");
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
      Rest (Files_manip.pre_commit_cmd "add"),
      "< mg -add \"filename\" > set the file/directory as to be saved on the next commit");
   ("-minus",
      Rest (Files_manip.pre_commit_cmd "minus"),
      "< mg -minus \"filename\" > remove the file/dir from the list of files to be commited. \n\
       Can be used to undo a -add, or to be more precise. \n\
       For instance you can -add a whole directory and then -minus just one file the dir contains.");
   ("-remove",
      Rest (Files_manip.pre_commit_cmd "remove"),
      "Remove the file/dir from the current copy branch.");
   ("-move",
      Rest_all Files_manip.cmd_move,
      "Move the file/dir both in the real working directory and in the current copy branch.");
   ("-commit",
      Unit subparser_commit,
      "Save the modifications in the repo.");
   ("-ls",
      Unit Tree.cmd_ls , 
      "Create a \"repo_tree.pdf\" file (using Graphviz's dot command).");
   ("-status",
      Unit Status.cmd_status , 
      "Show the status of the copy.");
   ("-restore",
      String Files_manip.cmd_restore ,
      "Restore the file requested (or the whole directory).");
   ]


let () = Arg.parse_dynamic speclist Tmp.aff_qlqch ""

let () = match !subparser with
  | "commit" -> Commit.cmd_commit ()
  | _ -> ()

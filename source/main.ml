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
   ("-init",
      Unit Basic_cmds.init , 
      "Create a new repository for the current working directory.");
   ("-remove",
      Unit Basic_cmds.remove ,
      "Remove the cwd repo.");
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
      String (Commit.cmd_add true),
      "< mg -add \"filename\" > set the file/directory as to be saved on the next commit");
   ("-minus",
      String (Commit.cmd_add false),
      "< mg -minus \"filename\" > remove the file/dir from the list of files to be commited. \n\
       Can be used to undo a -add, or to be more precise. \n\
       For instance you can -add a whole directory and then -minus just one file the dir contains.");
   ("-commit",
      Unit subparser_commit,
      "Save the modifications in the repo");
   ]
    
let () = Arg.parse_dynamic speclist Tmp.aff_qlqch ""

let () = match !subparser with
  | "commit" -> Commit.cmd_commit ()
  | _ -> ()

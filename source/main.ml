let () = Arg.parse                                                       
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
      "\"git -hash_file \"filename\"\" will store \"filename\" at .mongit/files/");
   ("-cat_file",
      String Basic_cmds.cat_file ,
      "\"git -cat_file \"sha_key\"\" will print out the file .mongit/files/sha_key");
   ]
  Tmp.aff_qlqch ""

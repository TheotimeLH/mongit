let () = Arg.parse                                                       
  (*["-debug", Unit (fun () -> Root.print_debug := true) , "blabla"]*)
  [("-debug", 
      Set Root.bool_print_debug , 
      "Print debug messages");
   ("-init",
      Unit Basic_cmds.init , 
      "Create a new repository for the current working directory.");
   ("-remove",
      Unit Basic_cmds.remove ,
      "Remove the cwd repo.")
   ]
  Tmp.aff_qlqch ""

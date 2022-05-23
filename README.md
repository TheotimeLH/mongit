# mg : a Git-like revision control system

## Warning :
Be careful, you need to use a ***Ocaml version 4.14.0*** or higher !

---

To use mg you must clone the source directory.
Then you just have to call `make`.

Afterwards you can use the mg program wherever you want. I advice you to start with `mg -help` and `mg -branch -help`. 

To update mg you just have to do a `mg -update`. You no longer need to interact with the Makefile.
To remove the build files you can do `make clean` and to reset everything you can do a `make clean_all`. 

### Details :
mg has a -update command that allows it to update itself from anywhere, but to do so it needs to know where the source file is, that's why the first `make` triggers a `make init`.

But to use mg the binary file must be in a directory accessible through the PATH. mg is not intrusive, it will ask you where you allow him to put the binary file. You can give any directory name (even the source dir). 
Then it will check if the given directory belongs to the PATH. If it doesn't, it will alert you and prompt you to add it to the PATH (but again, it doesn't do anything without your permission).

`make clean_all` erase the `make init`.

 

## Author :
Théotime Le Hellard, as a school project at the École Normale Supérieure
https://github.com/physikks/mongit

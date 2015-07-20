# README #

This config is a work in progress. I would recommend trying out bits and pieces of it and keeping the parts you like. 

### manual setup ###

* The 1st line of `init.el` needs to specify the absolute path of `user-defined/`
* I have all of the files in `user-defined` byte-compiled. This significantly sped up my emacs boot time. There's probably an automated way to do this, but it can be done manually with `M-x byte-compile-file`. It only needs to be done once per file. 
    * The function `byte-compile-current-buffer`, defined in `user-defined/defun.el`, that will automatically byte compile any `.el` file that has previously been byte compiled. And `user-defined/elisp.el` sets it to run every time a `.el` file is saved. With this a `.el` file only needs to be manually byte-compiled once, and it will automatically update every time after that.

### structure ###

* `init.el` 
    * specifies the path to `load-paths.el`
    * miscellaneous configuration
    * variables set by emacs
* `user-defined/loadpaths.el`
    * specifies all other loadpaths
* `user-defined/*`
    * everything besides `loadpaths.el` are variables and functions grouped by their relation to a particular mode or topic.
    * they will only be loaded if they are specified in `loadpaths.el`
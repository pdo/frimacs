Frimacs
=======

The *frimacs* package is intended to make it easier to work with and
understand the *FriCAS* computer algebra system.  It implements four
different major modes for the Emacs text editor:

1. frimacs-process-mode: for interaction with a running FriCAS process.

2. frimacs-help-mode: for displaying help information about the FriCAS system.

3. frimacs-input-mode: for editing FriCAS script (.input) files.

4. frimacs-spad-mode: for editing FriCAS library code written in the SPAD language.

These modes enable syntax highlighting to display package, domain &
category names (and their abbreviations) in distinct colours, and give
quick access to popup buffers displaying summary information about
these types and their operations.

Frimacs may most easily be installed, using the usual Emacs package
management tools, from the MELPA repository (when it becomes available
there soon).

Alternatively, Frimacs can be installed by cloning its source code
repository at:-

  http://github.com/pdo/frimacs.git

and then adding the line:-

  (load-file "<frimacs-local-repo>/frimacs.el")

to your ~/.emacs file, where the <frimacs-local-repo> component should
be replaced with the location of the code repository on your system.
You may also want to run "C-u M-x byte-recompile-directory" to compile
all the source files to bytecode.

Once the package is installed, files ending in .input, and .spad are
put into the appropriate mode, and there is an "M-x run-fricas"
command available to start an interactive FriCAS session.  Look into
the Frimacs menu that appears in these buffers to discover further
capabilities of the system.

There is also an ``extras'' directory containing optional extensions
to the frimacs system, such as the `ob-fricas' backend for Org-Babel.
To also load these extensions from your ~/.emacs file use:-

  (load-file "<frimacs-local-repo>/extras/frimacs-extras.el")

after frimacs itself is loaded.  Alternatively, use the Emacs package
system to install the extras directly.

Note: this Emacs package (frimacs) can be considered to be the
successor to the axiom-environment package.  Currently frimacs offers
essentially the same functionality as axiom-environment did, but with
all function and variable names changed appropriately.  This has been
done in a fit of honesty, to acknowledge the fact that nearly all
development has been done with FriCAS from day one, and that it should
be FriCAS that takes centre stage in future developments.

PDO, 27 May 2022

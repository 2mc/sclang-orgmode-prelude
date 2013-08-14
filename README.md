sclang-orgmode-prelude
======================

Personal folder for emacs-prelude package: setup of sclang, auto-load of packages in prelude folder (+later: dynsite orgmode-html publishing extension)

## To use:

- Download and install emacs-prelude. See instructions here: http://batsov.com/prelude/

- The present configuration requires that the prelude package be placed in folder named .emacs.d exactly as described in http://batsov.com/prelude/

- Copy the contents of the present repository inside folder `personal` of `.emacs.d`

- Start Emacs.

## Contents:

- `load-personal-folder-packages.el` : Automatically loads any packages that are contained in subfolders of the `personal` folder in the present prelude folder (`.emacs.d`). Only tries to load those folders which contain an .el file with the same name as the folder. For example: package `sclang` is loaded if the folder `personal` contains a subfolder named `sclang` which in turn contains a file `sclang.el`.

- `sclang-setup.el` : Initialize paths for running SuperCollider in Emacs.

- `sclang-snippets.el` : Define sclang-mode keyboard shortcuts for evaluating SuperCollider code regions separated by comment lines with semicolon: `//:`.

- `emacs-customization-general.el` : Diverse customizations, too small to put in separate files. Currently: Activate automatic re-opening of last session's files on startup; set Emacs default frame size to a larger size.

- `keyboard-binding.el` : My additions and modifications to keyboard bindings of Emacs.

Help keeping `~/.emacs.d` clean
===============================

The default paths used to store configuration files and persistent
data are not consistent across Emacs packages.  This isn't just a
problem with third-party packages but even with built-in packages.

Some packages put these files directly in `user-emacs-directory`
or `$HOME` or in a subdirectory of either of the two or elsewhere.
Furthermore sometimes file names are used that don't provide any
insight into what package might have created them.

This package sets out to fix this by changing the values of path
variables to put files in either `no-littering-etc-directory`
(defaulting to `~/.emacs.d/etc/`) or `no-littering-var-directory`
(defaulting to `~/.emacs.d/var/`), and by using descriptive file
names and subdirectories when appropriate.  This is similar to a
color-theme; a "path-theme" if you will.

We still have a long way to go until most built-in and many
third-party path variables are properly "themed".  Like a color-theme,
this package depends on user contributions to accomplish decent
coverage.  Pull requests are highly welcome.

Usage
-----

Load the feature `no-littering` as early as possible in your init
file.  Make sure you load it at least before you change any path
variables using some other method.

    (require 'no-littering)

If you would like to use base directories different from what
`no-littering` uses by default, then you have to set the respective
variables before loading the feature.

    (setq no-littering-etc-directory
          (expand-file-name "config/" user-emacs-directory))
    (setq no-littering-var-directory
          (expand-file-name "data/" user-emacs-directory))
    (require 'no-littering)

Conventions
-----------

### File names

If a package has only one persistent data file, then that is placed
in `no-littering-var-directory' itself.  If a package has multiple
files, then they are placed in a subdirectory, whose file name is
the name of the package.  Likewise for a package's configuration
files.

Additonally a file name should:

1. Share the prefix of the package that uses it.  For files that
   are located in a a subdirectory, the name of the directory counts
   as the prefix.

2. Reflect the name of the elisp variable that references it.  If
   you only know the name of the file, it should be trivial to find
   the variable that references that file in Emacs (e.g. to find
   its documentation).

3. Use appropriate extensions (if applicable) to signal the content
   of the file and to trigger the correct major-mode.

### Ordering and alignment

The code that sets the values of themed variables is split into two
groups.  The first group sets the value of variables that belong to
packages that are part of Emacs, and the second group is used for
variables that are defined by packages that are not part of Emacs.
Each of these lists is sorted alphabetically.  Please keep it that
way.

We attempt to align the value forms inside different `setq` forms.
If the symbol part for a particular variable is too long to allow
doing so, then don't worry about it and just break the alignment.
If it turns out that this happens very often, then we will adjust
the alignment eventually.

### Commit messages

Please theme each package using a separate commit and use commit
messages of the form "<package>: theme <variable".  If a package
has several path variables, then you should theme them all in one
commit.  If the variable names do not fit nicely on the summary
line, then use a message such as:

```
foo: theme variables

Theme `foo-config-file', `foo-cache-directory',
and `foo-persistent-file'.
```

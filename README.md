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

* File names are based on the name of the respective Emacs lisp
  variables and the name of the respective Emacs package.
   
* If the name of a path variable ends with `-file`, `-default-file`,
  `-directory`, `-default-directory`, `, or something similar, then
  that suffix is usually dropped from the file name.

* If applicable, the appropriate extension is added to the file name
  so that files are visited using the appropriate major-modes and also
  to provide a hint about the kind of data stored in the file.  E.g.
  if a file contains an S-expression, then the suffix should be `*.el`.

* If a package has only one data file, then that is usually placed in
  `no-littering-var-directory` itself.  Likewise if a package has only
  one config file, then that is placed in `no-littering-etc-directory`
  itself.
  
* If a package has multiple data (or config files), then those files
  are placed in a subdirectory of `no-littering-var-directory` (or
  `no-littering-var-directory`).
  
* If a subdirectory is used for a package's data (or config) file
  variables, then the name of the directory should match the name of
  the package in most cases.
  
* If the name of the package and the prefix of the variable do not
  match, then we prefer the name of the package.
  
* A package that provides a "framework" for other packages to use,
  then we may reuse its directories for other packages that make use
  of that framework or otherwise "extend" the "main package".  E.g. we
  place all `helm` related files in `helm/`.
  
* If a package only defines a single variable that specifies a data
  (or config) directory, then the directory name should never-the-less
  be just the package name.  E.g. the path used for
  `sx-cache-directory` from the `sx` package is `sx/cache/`, not
  `sx-cache/`.
  
* However if the name of the directory variable implies that the
  package won't ever define any data (or config) files that won't be
  placed in that directory, then we use a top-level directory.  E.g.
  when the name of the variable is `<package>-directory`, in which
  case we would use just `<package>/` as the path.

* File names share the prefix of the package where the variable is
  defined.  For files that are located in a a subdirectory, the name
  of the directory counts as the prefix.

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

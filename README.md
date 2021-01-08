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
variables to put configuration files in `no-littering-etc-directory`
(defaulting to "etc/" under `user-emacs-directory`, thus usually
"~/.emacs.d/etc/") and persistent data files in
`no-littering-var-directory` (defaulting to "var/" under
`user-emacs-directory`, thus usually "~/.emacs.d/var/"), and
by using descriptive file names and subdirectories when appropriate.
This is similar to a color-theme; a "path-theme" if you will.

We still have a long way to go until most built-in and many
third-party path variables are properly "themed".  Like a color-theme,
this package depends on user contributions to accomplish decent
coverage.  Pull requests are highly welcome (but please follow the
conventions described below and in the pull request template).

`no-littering` cannot help with moving existing files to the new
location.  You will have to move the files manually.  See issue
[#79](https://github.com/emacscollective/no-littering/issues/79)
for more information.

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

### Suggested Settings

If you use `recentf` then you might find it convenient to exclude all
of the files in the `no-littering` directories using something like
the following.

    (require 'recentf)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)

#### Auto-save settings

One of the most common types of files that Emacs creates automatically
is auto-save files.  By default, these appear in the current directory
of a visited file.  No-littering does not change this, but you can add
the following code to your `init.el` file to store these files in the
`var` directory:

    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

#### Saved customizations

Emacs will save [customizations] into your `init.el` file by default.
If you don't want that, you might want to store them in a sibling file
or even in the `etc/` directory:

    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
	;; or
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))

Conventions
-----------

### (A) File names

1. File names are based on the name of the respective Emacs lisp
   variables and the name of the respective Emacs package.

2. The name of the respective Emacs package should serve as the
   prefix of the file name, unless the file is in a subdirectory in
   which case the name of the subdirectory serves as the prefix.

3. If the name of the package and the prefix of the variable do not
   match, then we prefer the name of the package.

4. If the name of a path variable ends with `-file`, `-default-file`,
   `-directory`, `-default-directory`, or something similar, then that
   suffix is usually dropped from the file name.

5. If applicable, the appropriate extension is added to the file name
   so that files are visited using the appropriate major-modes and
   also to provide a hint about the kind of data stored in the file.
   E.g.  if a file contains an S-expression, then the suffix should be
   `*.el`.

### (B) File location and subdirectories

1. If a package has only one data file, then that is usually placed in
   `no-littering-var-directory` itself.  Likewise if a package has
   only one config file, then that is placed in
   `no-littering-etc-directory` itself.

2. If a package has multiple data (or config files), then those files
   are placed in a subdirectory of `no-littering-var-directory` (or
   `no-littering-etc-directory`).

3. If a subdirectory is used for a package's data (or config) file
   variables, then the name of the directory should match the name of
   the package in most cases. The subdirectory name may serve as the
   package prefix of the file name.

4. If a package provides a "framework" for other packages to use,
   then we may reuse its directories for other packages that make use
   of that framework or otherwise "extend" the "main package".
   E.g. we place all `helm` related files in `helm/`.

5. If a package only defines a single variable that specifies a data
   (or config) directory, then the directory name should
   nevertheless be just the package name.  E.g. the path used for
   `sx-cache-directory` from the `sx` package is `sx/cache/`, not
   `sx-cache/`.

6. However if the name of the directory variable implies that the
   package won't ever define any data (or config) files that won't be
   placed in that directory, then we use a top-level directory.  E.g.
   when the name of the variable is `<package>-directory`, in which
   case we would use just `<package>/` as the path.

### (C) Ordering and alignment

1. The code that sets the values of themed variables is split into two
   groups.  The first group sets the value of variables that belong to
   packages that are part of Emacs, and the second group is used for
   variables that are defined by packages that are not part of Emacs.

2. Each of these lists is sorted alphabetically (usually by variable
   name).  Please keep it that way.

3. We attempt to align the value forms inside different `setq` forms.
   If the symbol part for a particular variable is too long to allow
   doing so, then don't worry about it and just break the alignment.
   If it turns out that this happens very often, then we will adjust
   the alignment eventually.

### (D) Commit messages

1. Please theme each package using a separate commit and use commit
   messages of the form `PACKAGE: theme VARIABLE`.

2. If a package has several path variables, then you should theme them
   all in one commit.

3. If the variable names do not fit nicely on the summary line, then
   use a message such as:

```
foo: theme variables

Theme `foo-config-file', `foo-cache-directory',
and `foo-persistent-file'.
```

4. When appropriate add statements like the following to the commit
   message:

   - This file is used to store an s-expression.
   - This file is used to store raw text.
   - This is the only configuration/data file of the package.
   - This package does/doesn't take care of creating the containing
     directory if necessary. (If the package does not do it, then you
     should also fix that and submit an upstream pull request.)

5. If you are uncertain, then be explicit about it by adding a comment
   to the pull-request.

[customizations]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html

<!-- Local Variables: -->
<!-- fill-column: 70 -->
<!-- End: -->

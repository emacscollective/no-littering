;;; no-littering.el --- help keeping ~/.emacs.d clean  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2022  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/no-littering
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see https://www.gnu.org/licenses.

;;; Commentary:

;; Help keeping ~/.emacs.d clean.

;; The default paths used to store configuration files and persistent
;; data are not consistent across Emacs packages.  This isn't just a
;; problem with third-party packages but even with built-in packages.

;; Some packages put these files directly in `user-emacs-directory'
;; or $HOME or in a subdirectory of either of the two or elsewhere.
;; Furthermore sometimes file names are used that don't provide any
;; insight into what package might have created them.

;; This package sets out to fix this by changing the values of path
;; variables to put configuration files in `no-littering-etc-directory'
;; (defaulting to "etc/" under `user-emacs-directory', thus usually
;; "~/.emacs.d/etc/") and persistent data files in
;; `no-littering-var-directory' (defaulting to "var/" under
;; `user-emacs-directory', thus usually "~/.emacs.d/var/"), and
;; by using descriptive file names and subdirectories when appropriate.
;; This is similar to a color-theme; a "path-theme" if you will.

;; We still have a long way to go until most built-in and many third-
;; party path variables are properly "themed".  Like a color-theme,
;; this package depends on user contributions to accomplish decent
;; coverage.  Pull requests are highly welcome (but please follow the
;; conventions described below and in the pull request template).

;; `no-littering' cannot help with moving existing files to the new
;; location.  You will have to move the files manually.  See issue
;; #79 for more information.

;; Usage:

;; Load the feature `no-littering' as early as possible in your init
;; file.  Make sure you load it at least before you change any path
;; variables using some other method.
;;
;;   (require 'no-littering)

;; If you would like to use base directories different from what
;; `no-littering' uses by default, then you have to set the respective
;; variables before loading the feature.
;;
;;   (setq no-littering-etc-directory
;;         (expand-file-name "config/" user-emacs-directory))
;;   (setq no-littering-var-directory
;;         (expand-file-name "data/" user-emacs-directory))
;;   (require 'no-littering)

;; Conventions:

;; * A) File names
;;
;; 1. File names are based on the name of the respective Emacs Lisp
;;    variables and the name of the respective Emacs package.
;;
;; 2. The name of the respective Emacs package should serve as the
;;    prefix of the file name, unless the file is in a subdirectory in
;;    which case the name of the subdirectory serves as the prefix.
;;
;; 3. If the name of the package and the prefix of the variable do not
;;    match, then we prefer the name of the package.
;;
;; 4. If the name of a path variable ends with `-file`, `-default-file`,
;;    `-directory`, `-default-directory`, or something similar, then that
;;    suffix is usually dropped from the file name.
;;
;; 5. If applicable, the appropriate extension is added to the file name
;;    so that files are visited using the appropriate major-modes and
;;    also to provide a hint about the kind of data stored in the file.
;;    E.g.  if a file contains an S-expression, then the suffix should be
;;    `*.el`.

;; * B) File location and subdirectories
;;
;; 1. If a package has only one data file, then that is usually placed in
;;    `no-littering-var-directory` itself.  Likewise if a package has
;;    only one config file, then that is placed in
;;    `no-littering-etc-directory` itself.
;;
;; 2. If a package has multiple data (or config files), then those files
;;    are placed in a subdirectory of `no-littering-var-directory` (or
;;    `no-littering-etc-directory`).
;;
;; 3. If a subdirectory is used for a package's data (or config) file
;;    variables, then the name of the directory should match the name of
;;    the package in most cases.  The subdirectory name may serve as the
;;    package prefix of the file name.
;;
;; 4. If a package provides a "framework" for other packages to use,
;;    then we may reuse its directories for other packages that make use
;;    of that framework or otherwise "extend" the "main package".
;;    E.g. we place all `helm` related files in `helm/`.
;;
;; 5. If a package only defines a single variable that specifies a data
;;    (or config) directory, then the directory name should
;;    nevertheless be just the package name.  E.g. the path used for
;;    `sx-cache-directory` from the `sx` package is `sx/cache/`, not
;;    `sx-cache/`.
;;
;; 6. However if the name of the directory variable implies that the
;;    package won't ever define any data (or config) files that won't be
;;    placed in that directory, then we use a top-level directory.  E.g.
;;    when the name of the variable is `<package>-directory`, in which
;;    case we would use just `<package>/` as the path.

;; * C) Ordering and alignment
;;
;; 1. The code that sets the values of themed variables is split into two
;;    groups.  The first group sets the value of variables that belong to
;;    packages that are part of Emacs, and the second group is used for
;;    variables that are defined by packages that are not part of Emacs.
;;
;; 2. Each of these lists is sorted alphabetically (usually by variable
;;    name).  Please keep it that way.
;;
;; 3. We attempt to align the value forms inside different `setq` forms.
;;    If the symbol part for a particular variable is too long to allow
;;    doing so, then don't worry about it and just break the alignment.
;;    If it turns out that this happens very often, then we will adjust
;;    the alignment eventually.

;; * D) Commit messages
;;
;; 1. Please theme each package using a separate commit and use commit
;;    messages of the form "<package>: theme <variable".
;;
;; 2. If a package has several path variables, then you should theme them
;;    all in one commit.
;;
;; 3. If the variable names do not fit nicely on the summary line, then
;;    use a message such as:
;;
;;      foo: theme variables
;;
;;      Theme `foo-config-file', `foo-cache-directory',
;;      and `foo-persistent-file'.
;; 4. When appropriate add statements like the following to the commit
;;    message:
;;
;;    - This file is used to store an s-expression.
;;    - This file is used to store raw text.
;;    - This is the only configuration/data file of the package.
;;    - This package does/doesn't take care of creating the containing
;;      directory if necessary. (If the package does not do it, then you
;;      should also fix that and submit an upstream pull request.)
;;
;; 5. If you are uncertain, then be explicit about it by adding a comment
;;    to the pull-request.

;;; Code:

(require 'cl-lib)

(defvar no-littering-etc-directory
  (expand-file-name (convert-standard-filename "etc/") user-emacs-directory)
  "The directory where packages place their configuration files.
This variable has to be set before `no-littering' is loaded.")

(defvar no-littering-var-directory
  (expand-file-name (convert-standard-filename "var/") user-emacs-directory)
  "The directory where packages place their persistent data files.
This variable has to be set before `no-littering' is loaded.")

;;;###autoload
(defun no-littering-expand-etc-file-name (file)
  "Expand filename FILE relative to `no-littering-etc-directory'."
  (expand-file-name (convert-standard-filename file)
                    no-littering-etc-directory))

;;;###autoload
(defun no-littering-expand-var-file-name (file)
  "Expand filename FILE relative to `no-littering-var-directory'."
  (expand-file-name (convert-standard-filename file)
                    no-littering-var-directory))

(cl-letf (((symbol-function 'etc)
           (symbol-function #'no-littering-expand-etc-file-name))
          ((symbol-function 'var)
           (symbol-function #'no-littering-expand-var-file-name)))
  (make-directory no-littering-etc-directory t)
  (make-directory no-littering-var-directory t)
  (with-no-warnings ; many of these variables haven't been defined yet

;;; Built-in packages

    (setq abbrev-file-name                 (etc "abbrev.el"))
    (setq auto-insert-directory            (etc "auto-insert/"))
    (setq auto-save-list-file-prefix       (var "auto-save/sessions/"))
    (setq backup-directory-alist           (list (cons "." (var "backup/"))))
    (setq bookmark-default-file            (var "bookmark-default.el"))
    (setq calc-settings-file               (etc "calc-settings.el"))
    (eval-after-load 'desktop
      `(make-directory ,(var "desktop/") t))
    (setq desktop-dirname                  (var "desktop/"))
    (setq desktop-path                     (list desktop-dirname))
    (setq diary-file                       (var "diary"))
    (setq ecomplete-database-file          (var "ecomplete-database.el"))
    (setq ede-project-placeholder-cache-file (var "ede-projects.el"))
    (eval-after-load 'erc
      `(make-directory ,(var "erc/dcc/") t))
    (setq erc-dcc-get-default-directory    (var "erc/dcc/"))
    (setq erc-log-channels-directory       (var "erc/log-channels/"))
    (eval-after-load 'eshell
      `(make-directory ,(etc "eshell/") t))
    (setq eshell-aliases-file              (etc "eshell/aliases"))
    (setq eshell-directory-name            (var "eshell/"))
    (setq eudc-options-file                (etc "eudc-options.el"))
    (eval-after-load 'eww
      `(make-directory ,(var "eww/") t))
    (setq eww-bookmarks-directory          (var "eww/"))
    (setq filesets-menu-cache-file         (var "filesets-menu-cache.el"))
    (setq gamegrid-user-score-file-directory (var "gamegrid-user-score/"))
    (eval-after-load 'gnus
      `(make-directory ,(var "gnus/dribble/") t))
    (setq gnus-dribble-directory           (var "gnus/dribble/"))
    (setq gnus-init-file                   (etc "gnus/init.el"))
    (setq ido-save-directory-list-file     (var "ido-save-directory-list.el"))
    (setq image-dired-db-file              (var "image-dired/db.el"))
    (setq image-dired-dir                  (var "image-dired/"))
    (setq image-dired-gallery-dir          (var "image-dired/gallery/"))
    (setq image-dired-temp-image-file      (var "image-dired/temp-image"))
    (setq image-dired-temp-rotate-image-file (var "image-dired/temp-rotate-image"))
    (setq kkc-init-file-name               (var "kkc-init.el"))
    (setq multisession-directory           (var "multisession/"))
    (eval-after-load 'newsticker
      `(make-directory ,(var "newsticker/") t))
    (setq newsticker-cache-filename        (var "newsticker/cache.el"))
    (setq newsticker-dir                   (var "newsticker/data/"))
    (setq nsm-settings-file                (var "nsm-settings.el"))
    (eval-after-load 'org
      `(make-directory ,(var "org/") t))
    (setq org-clock-persist-file           (var "org/clock-persist.el"))
    (setq org-id-locations-file            (var "org/id-locations.el"))
    (setq org-persist-directory            (var "org/persist/"))
    (setq org-publish-timestamp-directory  (var "org/timestamps/"))
    (setq project-list-file                (var "project-list.el"))
    (setq quickurl-url-file                (var "quickurl-url.el"))
    (setq rcirc-log-directory              (var "rcirc-log/"))
    (setq recentf-save-file                (var "recentf-save.el"))
    (setq remember-data-file               (var "remember/data"))
    (setq remember-data-directory          (var "remember/data.d/"))
    (setq save-place-file                  (var "save-place.el"))
    (setq savehist-file                    (var "savehist.el"))
    (setq srecode-map-save-file            (var "srecode-map.el"))
    (setq semanticdb-default-save-directory (var "semantic/"))
    (eval-after-load 'shadowfile
      `(make-directory ,(var "shadow/") t))
    (setq shadow-info-file                 (var "shadow/info.el"))
    (setq shadow-todo-file                 (var "shadow/todo.el"))
    (setq shared-game-score-directory      (var "shared-game-score/"))
    (setq timeclock-file                   (var "timeclock"))
    (setq tramp-auto-save-directory        (var "tramp/auto-save/"))
    (setq tramp-persistency-file-name      (var "tramp/persistency.el"))
    (setq type-break-file-name             (var "type-break.el"))
    (setq url-cache-directory              (var "url/cache/"))
    (setq url-configuration-directory      (var "url/"))
    (setq url-cookie-file                  (var "url/cookies.el"))
    (setq url-history-file                 (var "url/history.el"))

;;; Third-party packages

    (setq ac-comphist-file                 (var "ac-comphist.el"))
    (setq amx-save-file                    (var "amx-save.el"))
    (setq anaconda-mode-installation-directory (var "anaconda-mode/"))
    (setq annotate-file                    (var "annotations.el"))
    (setq async-byte-compile-log-file      (var "async-bytecomp.log"))
    (setq auto-package-update-last-update-day-path (var "auto-package-update-last-update-day"))
    (eval-after-load 'bbdb
      `(make-directory ,(var "bbdb/") t))
    (setq bbdb-file                        (var "bbdb/bbdb.el"))
    (setq bbdb-vcard-directory             (var "bbdb/vcard/"))
    (setq bm-repository-file               (var "bm-repository.el"))
    (eval-after-load 'bookmark+-1
      `(make-directory ,(var "bmkp/") t))
    (setq bmkp-bmenu-commands-file         (var "bmkp/bmenu-commands.el"))
    (setq bmkp-current-bookmark-file       (var "bmkp/current-bookmark.el"))
    (setq bmkp-last-bookmark-file          (var "bmkp/last-bookmark.el"))
    (setq bookiez-file                     (var "bookiez"))
    (setq cider-repl-history-file          (var "cider-repl-history.el"))
    (setq code-review-db-database-file     (var "code-review/database.sqlite"))
    (setq code-review-download-dir         (var "code-review/downloads/"))
    (setq code-review-log-file             (var "code-review/log"))
    (eval-after-load 'command-log-mode
      `(make-directory ,(var "command-log-mode-logging/") t))
    (setq clm/logging-dir                  (var "command-log-mode/logging/"))
    (eval-after-load 'company
      `(make-directory ,(var "company/") t))
    (setq company-statistics-file          (var "company/statistics.el"))
    (setq company-tabnine-binaries-folder  (var "company/tabnine-binaries"))
    (setq conventional-changelog-tmp-dir   (var "conventional-changelog/"))
    (setq dap-breakpoints-file             (var "dap/breakpoints.el"))
    (setq dap-java-test-runner             (var "lsp-java/eclipse.jdt.ls/test-runner/junit-platform-console-standalone.jar"))
    (setq dap-utils-extension-path         (var "dap/extensions/"))
    (setq debbugs-gnu-persistency-file     (var "debbugs.el"))
    (setq devdocs-browser-cache-directory  (var "devdocs/browser-cache/"))
    (setq devdocs-data-dir                 (var "devdocs/data/"))
    (setq dired-recent-directories-file    (var "dired-recent-directories.el"))
    (setq elbank-data-file                 (var "elbank-data.el"))
    (setq elmo-msgdb-directory             (var "elmo/"))
    (setq elmo-split-log-file              (var "elmo/split-log"))
    (eval-after-load 'elfeed
      `(make-directory ,(var "elfeed/") t))
    (setq elfeed-db-directory              (var "elfeed/db/"))
    (setq elfeed-enclosure-default-dir     (var "elfeed/enclosures/"))
    (setq elfeed-score-score-file          (etc "elfeed/score/score.el"))
    (setq elpher-bookmarks-file            (var "elpher-bookmarks.el"))
    (eval-after-load 'x-win
      (let ((session-dir (var "emacs-session/")))
        `(progn
           (make-directory ,session-dir t)
           (defun emacs-session-filename (session-id)
             "Construct a filename to save the session in based on SESSION-ID.
This function overrides the one on `x-win' to use `no-littering'
directories."
             (expand-file-name session-id ,session-dir)))))
    (setq emms-directory                   (var "emms/"))
    (eval-after-load 'emojify
      `(make-directory ,(var "emojify/") t))
    (setq emojify-emojis-dir               (var "emojify/"))
    (setq epkg-repository                  (var "epkgs/"))
    (setq equake-persistent-display-file   (var "equake-persistent-display"))
    (setq forge-database-file              (var "forge/database.sqlite"))
    (setq forge-post-directory             (var "forge/posts/"))
    (setq geben-temporary-file-directory   (var "geben/"))
    (eval-after-load 'geiser
      `(make-directory ,(var "geiser/") t))
    (setq geiser-repl-history-filename     (var "geiser/repl-history"))
    (setq hackernews-visited-links-file    (var "hackernews/visited-links.el"))
    (eval-after-load 'helm
      `(make-directory ,(var "helm/") t))
    (setq helm-adaptive-history-file       (var "helm/adaptive-history.el"))
    (setq helm-backup-path                 (var "helm/backup/"))
    (setq helm-github-stars-cache-file     (var "helm/github-stars-cache.el"))
    (setq helm-net-curl-log-file           (var "helm/helm-curl.log"))
    (setq historian-save-file              (var "historian-save.el"))
    (setq indium-workspace-file            (var "indium/workspaces.el"))
    (setq irfc-directory                   (var "irfc/"))
    (setq irony-user-dir                   (var "irony/"))
    (setq jabber-avatar-cache-directory    (var "jabber/avatar-cache"))
    (eval-after-load 'jabber
      `(make-directory ,(var "jabber/avatar-cache/") t))
    (setq jabber-history-dir               (var "jabber/history"))
    (eval-after-load 'jabber
      `(make-directory ,(var "jabber/history/") t))
    (setq keyfreq-file                     (var "keyfreq.el"))
    (setq keyfreq-file-lock                (var "keyfreq.lock"))
    (setq libbcel-oauth-store-filename     (var "libbcel-oauth-store.el.gpg"))
    (setq litable-list-file                (var "litable-list.el"))
    (setq logview-cache-filename           (var "logview-cache"))
    (setq logview-views-file               (etc "logview-views"))
    (eval-after-load 'lookup
      `(make-directory ,(etc "lookup/") t))
    (setq lookup-init-directory            (etc "lookup/"))
    (setq lsp-eslint-library-choices-file  (var "lsp/eslint-library-choices.el"))
    (setq lsp-python-ms-dir                (var "lsp-python-ms/"))
    (eval-after-load 'lsp-mode
      `(make-directory ,(var "lsp/") t))
    (setq lsp-server-install-dir           (var "lsp/server/"))
    (setq lsp-session-file                 (var "lsp/session.el"))
    (setq lsp-java-workspace-dir           (var "lsp-java/workspace/"))
    (setq lsp-java-server-install-dir      (var "lsp-java/eclipse.jdt.ls/server/"))
    (setq magithub-dir                     (var "magithub/"))
    (setq magithub-cache-file              (var "magithub/cache.el"))
    (setq mc/list-file                     (var "mc-list.el"))
    (setq meghanada-server-install-dir     (var "meghanada/"))
    (setq multi-compile-history-file       (var "multi-compile-history.el"))
    (setq nix-buffer-directory-name        (var "nix-buffer/"))
    ;; The value of this variable MUST NOT end with ".el" but the
    ;; actual file name MUST end with ".el".  Use "git blame" for
    ;; more information.
    (setq notmuch-init-file                (etc "notmuch-init"))
    (setq nov-save-place-file              (var "nov-save-place.el"))
    (setq omnisharp-cache-directory        (var "omnisharp/cache"))
    (setq org-gcal-dir                     (var "org/gcal/"))
    (eval-after-load 'org-caldav
      `(make-directory ,(var "org/caldav/save") t))
    (setq org-caldav-backup-file           (var "org/caldav/backup.org"))
    (setq org-caldav-save-directory        (var "org/caldav/save"))
    (setq org-journal-cache-file           (var "org/journal-cache.el"))
    (setq org-recent-headings-save-file    (var "org/recent-headings.el"))
    (setq org-registry-file                (var "org/registry.el"))
    (setq org-roam-db-location             (var "org/org-roam.db"))
    (setq package-quickstart-file          (var "package-quickstart.el"))
    (setq pandoc-data-dir                  (etc "pandoc-mode/"))
    (setq pcache-directory                 (var "pcache/"))
    (setq pdf-view-restore-filename        (var "pdf-view-restore.el"))
    (setq persist--directory-location      (var "persist/"))
    (setq persistent-scratch-save-file     (var "persistent-scratch.el"))
    (setq persp-save-dir                   (var "persp-mode/"))
    (eval-after-load 'projectile
      `(make-directory ,(var "projectile/") t))
    (setq prescient-save-file              (var "prescient-save.el"))
    (setq projectile-cache-file            (var "projectile/cache.el"))
    (setq projectile-known-projects-file   (var "projectile/known-projects.el"))
    (setq psession-elisp-objects-default-directory (var "psession/"))
    (setq purpose-default-layout-file      (etc "window-purpose/default-layout.el"))
    (setq purpose-layout-dirs              (list (etc "window-purpose/layouts/")))
    (setq pyim-dcache-directory            (var "pyim/dcache/"))
    (setq quack-dir                        (var "quack/"))
    (setq rfc-mode-directory               (var "rfc-mode/"))
    (setq request-storage-directory        (var "request/storage/"))
    (setq rime-user-data-dir               (var "rime/"))
    (setq rmh-elfeed-org-files             (list (var "elfeed/rmh-elfeed.org")))
    (setq runner-init-file                 (var "runner-init.el"))
    (setq save-kill-file-name              (var "save-kill.el"))
    (setq save-visited-files-location      (var "save-visited-files-location"))
    (eval-after-load 'sly
      `(make-directory ,(var "sly/") t))
    (setq sly-mrepl-history-file-name      (var "sly/mrepl-history"))
    (setq smex-save-file                   (var "smex-save.el"))
    (setq speed-type-gb-dir                (var "speed-type/"))
    (eval-after-load 'sx
      `(make-directory ,(var "sx/cache/") t))
    (setq sx-cache-directory               (var "sx/cache/"))
    (setq tldr-directory-path              (var "tldr/"))
    (setq transient-history-file           (var "transient/history.el"))
    (setq transient-levels-file            (etc "transient/levels.el"))
    (setq transient-values-file            (etc "transient/values.el"))
    (setq treemacs-persist-file            (var "treemacs/persist.org"))
    (setq treemacs-last-error-persist-file (var "treemacs/persist-last-error.org"))
    (setq undo-fu-session-directory        (var "undo-fu-session/"))
    (setq undohist-directory               (var "undohist/"))
    (setq undo-tree-history-directory-alist (list (cons "." (var "undo-tree-hist/"))))
    (setq user-emacs-ensime-directory      (var "ensime/"))
    (setq vimish-fold-dir                  (var "vimish-fold/"))
    (eval-after-load 'wl
      `(make-directory ,(etc "wanderlust") t))
    (setq wl-init-file                     (etc "wanderlust/init.el"))
    (setq wl-folders-file                  (etc "wanderlust/folders"))
    (setq wl-address-file                  (etc "wanderlust/address"))
    (setq wl-alias-file                    (etc "wanderlust/alias"))
    (setq wl-x-face-file                   (etc "wanderlust/x-face"))
    (setq wl-temporary-file-directory      (var "wanderlust-tmp"))
    (setq x86-lookup-cache-directory       (var "x86-lookup/cache/"))
    (eval-after-load 'xkcd
      `(make-directory ,(var "xkcd/") t))
    (setq xkcd-cache-dir                   (var "xkcd/"))
    (eval-after-load 'yasnippet
      `(make-directory ,(etc "yasnippet/snippets/") t))
    (setq yas-snippet-dirs                 (list (etc "yasnippet/snippets/")))
    ))

;;; _
(provide 'no-littering)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; no-littering.el ends here

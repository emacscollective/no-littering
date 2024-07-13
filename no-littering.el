;;; no-littering.el --- Help keeping ~/.config/emacs clean  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.no-littering@jonas.bernoulli.dev>
;; Homepage: https://github.com/emacscollective/no-littering
;; Keywords: convenience

;; Package-Requires: ((emacs "25.1") (compat "30.0.0.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Help keeping ~/.config/emacs clean.

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
;; "~/.config/emacs/etc/") and persistent data files in
;; `no-littering-var-directory' (defaulting to "var/" under
;; `user-emacs-directory', thus usually "~/.emacs.d/var/"), and
;; by using descriptive file names and subdirectories when appropriate.
;; This is similar to a color-theme; a "path-theme" if you will.

;; We still have a long way to go until most built-in and many third-
;; party path variables are properly "themed".  Like a color-theme,
;; this package depends on user contributions to accomplish decent
;; coverage.  Pull requests are highly welcome (but please follow the
;; conventions described below and in the pull request template).

;; This package does not automatically migrate existing files to their
;; new locations, but unless you want to, you also do not have to do
;; it completely by hand.  The contributed "migrate.org" provides some
;; guidance and tools to help with the migration.

;;;; Usage

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

;; For additional optional settings see "README.org".

;;;; Conventions

;; Please see the "README.org" file in this repository or the exported
;; version at https://emacsmirror.net/manual/no-littering.html.

;;; Code:

(require 'cl-lib)
(require 'compat)

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
    (setq bookmark-default-file            (var "bookmark-default.el"))
    (setq calc-settings-file               (etc "calc-settings.el"))
    (setq desktop-dirname                  (var "desktop/"))
    (setq desktop-path                     (list desktop-dirname))
    (setq diary-file                       (var "diary"))
    (setq ecomplete-database-file          (var "ecomplete-database.el"))
    (setq ede-project-placeholder-cache-file (var "ede-projects.el"))
    (setq erc-dcc-get-default-directory    (var "erc/dcc/"))
    (setq erc-log-channels-directory       (var "erc/log-channels/"))
    (setq erc-startup-file-list            (list (etc "erc/startup.el") (etc "erc/startup") ".ercrc.el" ".ercrc"))
    (setq eshell-aliases-file              (etc "eshell/aliases"))
    (setq eshell-directory-name            (var "eshell/"))
    (setq eshell-login-script              (etc "eshell/login"))
    (setq eshell-rc-script                 (etc "eshell/rc"))
    (setq eudc-options-file                (etc "eudc-options.el"))
    (setq eww-bookmarks-directory          (var "eww/"))
    (setq filesets-menu-cache-file         (var "filesets-menu-cache.el"))
    (setq gamegrid-user-score-file-directory (var "gamegrid-user-score/"))
    (setq gnus-dribble-directory           (var "gnus/dribble/"))
    (setq gnus-init-file                   (etc "gnus/init.el"))
    ;; Gnus hardcodes newsrc.eld to be based on gnus-startup-file.
    (setq gnus-startup-file                (etc "gnus/newsrc"))
    (setq ido-save-directory-list-file     (var "ido-save-directory-list.el"))
    (setq ielm-history-file-name           (var "ielm-history.eld"))
    (setq image-dired-db-file              (var "image-dired/db.el"))
    (setq image-dired-dir                  (var "image-dired/"))
    (setq image-dired-gallery-dir          (var "image-dired/gallery/"))
    (setq image-dired-temp-image-file      (var "image-dired/temp-image"))
    (setq image-dired-temp-rotate-image-file (var "image-dired/temp-rotate-image"))
    (setq Info-saved-history-file          (var "info-saved-history.eld"))
    (setq kkc-init-file-name               (var "kkc-init.el"))
    (setq multisession-directory           (var "multisession/"))
    (setq newsticker-cache-filename        (var "newsticker/cache.el"))
    (setq newsticker-dir                   (var "newsticker/data/"))
    (setq nsm-settings-file                (var "nsm-settings.el"))
    (setq org-clock-persist-file           (var "org/clock-persist.el"))
    (setq org-id-locations-file            (var "org/id-locations.el"))
    (setq org-persist-directory            (var "org/persist/"))
    (setq org-publish-timestamp-directory  (var "org/timestamps/"))
    (setq persist--directory-location      (var "persist/"))
    (setq project-list-file                (var "project-list.el"))
    (setq quickurl-url-file                (var "quickurl-url.el"))
    (setq rcirc-log-directory              (var "rcirc-log/"))
    (setq recentf-save-file                (var "recentf-save.el"))
    (setq remember-data-directory          (var "remember/data.d/"))
    (setq remember-data-file               (var "remember/data"))
    (setq save-place-file                  (var "save-place.el"))
    (setq savehist-file                    (var "savehist.el"))
    (setq semanticdb-default-save-directory (var "semantic/"))
    (setq shadow-info-file                 (var "shadow/info.el"))
    (setq shadow-todo-file                 (var "shadow/todo.el"))
    (setq shared-game-score-directory      (var "shared-game-score/"))
    (setq srecode-map-save-file            (var "srecode-map.el"))
    (setq timeclock-file                   (var "timeclock"))
    (setq tramp-auto-save-directory        (var "tramp/auto-save/"))
    (setq tramp-persistency-file-name      (var "tramp/persistency.el"))
    (setq type-break-file-name             (var "type-break.el"))
    (setq url-cache-directory              (var "url/cache/"))
    (setq url-configuration-directory      (var "url/"))
    (setq url-cookie-file                  (var "url/cookies.el"))
    (setq url-history-file                 (var "url/history.el"))

    (eval-after-load 'desktop     '(make-directory desktop-dirname t))
    (eval-after-load 'erc         '(make-directory erc-dcc-get-default-directory t))
    (eval-after-load 'eshell      '(make-directory eshell-directory-name t))
    (eval-after-load 'eww         '(make-directory eww-bookmarks-directory t))
    (eval-after-load 'gnus        `(make-directory ,(etc "gnus/") t))
    (eval-after-load 'gnus        '(make-directory gnus-dribble-directory t))
    (eval-after-load 'newsticker  `(make-directory ,(var "newsticker/") t))
    (eval-after-load 'org         `(make-directory ,(var "org/") t))
    (eval-after-load 'shadowfile  `(make-directory ,(var "shadow/") t))

;;; Third-party packages

    (setq abm-file                         (var "autobookmarks.el"))
    (setq ac-comphist-file                 (var "ac-comphist.el"))
    (setq amx-save-file                    (var "amx-save.el"))
    (setq anaconda-mode-installation-directory (var "anaconda-mode/"))
    (setq annotate-file                    (var "annotations.el"))
    (setq async-byte-compile-log-file      (var "async-bytecomp.log"))
    (setq auto-package-update-last-update-day-path (var "auto-package-update-last-update-day"))
    (setq bbdb-file                        (var "bbdb/bbdb.el"))
    (setq bbdb-vcard-directory             (var "bbdb/vcard/"))
    (setq binky-cache-directory            (var "binky/cache/"))
    (setq blamer-avatar-folder             (var "blamer/avatars/"))
    (setq bm-repository-file               (var "bm-repository.el"))
    (setq bmkp-bmenu-commands-file         (var "bmkp/bmenu-commands.el"))
    (setq bmkp-current-bookmark-file       (var "bmkp/current-bookmark.el"))
    (setq bmkp-last-bookmark-file          (var "bmkp/last-bookmark.el"))
    (setq bookiez-file                     (var "bookiez"))
    (setq cider-repl-history-file          (var "cider-repl-history.el"))
    (setq clm/logging-dir                  (var "command-log-mode/logging/"))
    (setq code-review-db-database-file     (var "code-review/database.sqlite"))
    (setq code-review-download-dir         (var "code-review/downloads/"))
    (setq code-review-log-file             (var "code-review/log"))
    (setq company-statistics-file          (var "company/statistics.el"))
    (setq company-tabnine-binaries-folder  (var "company/tabnine-binaries"))
    (setq conventional-changelog-tmp-dir   (var "conventional-changelog/"))
    (setq copilot-install-dir              (var "copilot/"))
    (setq dap-breakpoints-file             (var "dap/breakpoints.el"))
    (setq dap-java-test-runner             (var "lsp-java/eclipse.jdt.ls/test-runner/junit-platform-console-standalone.jar"))
    (setq dap-utils-extension-path         (var "dap/extensions/"))
    (setq dape-adapter-dir                 (var "dape-adapters/"))
    (setq debbugs-gnu-persistency-file     (var "debbugs.el"))
    (setq detached-db-directory            (var "detached/db/"))
    (setq detached-session-directory       (var "detached/sessions/"))
    (setq devdocs-browser-cache-directory  (var "devdocs/browser-cache/"))
    (setq devdocs-data-dir                 (var "devdocs/data/"))
    (setq dired-recent-directories-file    (var "dired-recent-directories.el"))
    (setq dirvish-cache-dir                (var "dirvish/cache"))
    (setq elbank-data-file                 (var "elbank-data.el"))
    (setq elfeed-autotag-files             (list (etc "elfeed/autotags.org")))
    (setq elfeed-db-directory              (var "elfeed/db/"))
    (setq elfeed-enclosure-default-dir     (var "elfeed/enclosures/"))
    (setq elfeed-score-score-file          (etc "elfeed/score/score.el"))
    (setq elfeed-score-rule-stats-file     (var "elfeed/score/score-rule-stats.eld"))
    (setq elgrep-data-file                 (var "elgrep-data.el"))
    (setq elisp-autofmt-cache-directory    (var "elisp-autofmt/cache/"))
    (setq elmo-msgdb-directory             (var "elmo/"))
    (setq elmo-split-log-file              (var "elmo/split-log"))
    (setq elpher-bookmarks-file            (var "elpher-bookmarks.el"))
    (setq emacs-gc-stats-file              (var "emacs-gc-stats.eld"))
    (setq ement-sessions-file              (var "ement-sessions.el"))
    (setq emms-directory                   (var "emms/"))
    (setq emojify-emojis-dir               (var "emojify/"))
    (setq epkg-repository                  (var "epkgs/"))
    (setq equake-persistent-display-file   (var "equake-persistent-display"))
    (setq fontaine-latest-state-file       (var "fontaine-latest-state.eld"))
    (setq forge-database-file              (var "forge/database.sqlite"))
    (setq forge-post-directory             (var "forge/posts/"))
    (setq geben-temporary-file-directory   (var "geben/"))
    (setq geiser-repl-history-filename     (var "geiser/repl-history"))
    (setq gnus-notes-file                  (var "gnus-notes/articles.el"))
    (setq gnus-notes-top-dir               (var "gnus-notes/"))
    (setq gptel-crowdsourced-prompts-file  (var "gptel-crowdsourced-prompts.csv"))
    (setq grammalecte-settings-file        (etc "grammalecte-settings.el"))
    (setq hackernews-visited-links-file    (var "hackernews/visited-links.el"))
    (setq harpoon-cache-file               (var "harpoon/"))
    (setq helm-adaptive-history-file       (var "helm/adaptive-history.el"))
    (setq helm-backup-path                 (var "helm/backup/"))
    (setq helm-github-stars-cache-file     (var "helm/github-stars-cache.el"))
    (setq helm-net-curl-log-file           (var "helm/helm-curl.log"))
    (setq historian-save-file              (var "historian-save.el"))
    (setq indium-workspace-file            (var "indium/workspaces.el"))
    (setq irfc-directory                   (var "irfc/"))
    (setq irony-user-dir                   (var "irony/"))
    (setq jabber-avatar-cache-directory    (var "jabber/avatar-cache"))
    (setq jabber-history-dir               (var "jabber/history"))
    (setq keyfreq-file                     (var "keyfreq.el"))
    (setq keyfreq-file-lock                (var "keyfreq.lock"))
    (setq libbcel-oauth-store-filename     (var "libbcel-oauth-store.el.gpg"))
    (setq linkmarks-file                   (var "linkmarks.org"))
    (setq litable-list-file                (var "litable-list.el"))
    (setq logview-cache-filename           (var "logview-cache"))
    (setq logview-views-file               (etc "logview-views"))
    (setq lookup-init-directory            (etc "lookup/"))
    (setq lsp-clojure-workspace-dir        (var "lsp-clojure/workspace/"))
    (setq lsp-eslint-library-choices-file  (var "lsp/eslint-library-choices.el"))
    (setq lsp-java-server-install-dir      (var "lsp-java/eclipse.jdt.ls/server/"))
    (setq lsp-java-workspace-dir           (var "lsp-java/workspace/"))
    (setq lsp-ltex-user-rules-path         (var "lsp-ltex/"))
    (setq lsp-python-ms-dir                (var "lsp-python-ms/"))
    (setq lsp-server-install-dir           (var "lsp/server/"))
    (setq lsp-session-file                 (var "lsp/session.el"))
    (setq magithub-cache-file              (var "magithub/cache.el"))
    (setq magithub-dir                     (var "magithub/"))
    (setq mastodon-client--token-file      (var "mastodon-client--token"))
    (setq mc/list-file                     (var "mc-list.el"))
    (setq meghanada-server-install-dir     (var "meghanada/"))
    (setq multi-compile-history-file       (var "multi-compile-history.el"))
    (setq nix-buffer-directory-name        (var "nix-buffer/"))
    (setq nomad-tramp-script-directory     (var "nomad-tramp/"))
    ;; The value of this variable MUST NOT end with ".el" but the
    ;; actual file name MUST end with ".el".  Use "git blame" for
    ;; more information.
    (setq notmuch-init-file                (etc "notmuch-init"))
    (setq nov-save-place-file              (var "nov-save-place.el"))
    (setq omnisharp-cache-directory        (var "omnisharp/cache"))
    (setq org-caldav-backup-file           (var "org/caldav/backup.org"))
    (setq org-caldav-save-directory        (var "org/caldav/save"))
    (setq org-gcal-dir                     (var "org/gcal/"))
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
    (setq prescient-save-file              (var "prescient-save.el"))
    (setq projectile-cache-file            (var "projectile/cache.el"))
    (setq projectile-known-projects-file   (var "projectile/known-projects.el"))
    (setq psession-elisp-objects-default-directory (var "psession/"))
    (setq purpose-default-layout-file      (etc "window-purpose/default-layout.el"))
    (setq purpose-layout-dirs              (list (etc "window-purpose/layouts/")))
    (setq pyim-dcache-directory            (var "pyim/dcache/"))
    (setq quack-dir                        (var "quack/"))
    (setq racket-repl-command-file         (etc "racket-mode/repl.rkt"))
    (setq racket-repl-history-directory    (var "racket-mode/repl-history/"))
    (setq request-storage-directory        (var "request/storage/"))
    (setq rfc-mode-directory               (var "rfc-mode/"))
    (setq rime-user-data-dir               (var "rime/"))
    (setq rmh-elfeed-org-files             (list (etc "elfeed/rmh-elfeed.org")))
    (setq runner-init-file                 (var "runner-init.el"))
    (setq save-kill-file-name              (var "save-kill.el"))
    (setq save-visited-files-location      (var "save-visited-files-location"))
    (setq session-save-file                (var "session.el"))
    (setq shell-maker-history-path         (var "shell-maker/"))
    (setq sly-mrepl-history-file-name      (var "sly/mrepl-history"))
    (setq smex-save-file                   (var "smex-save.el"))
    (setq speed-type-gb-dir                (var "speed-type/"))
    (setq spell-fu-directory               (var "spell-fu/"))
    (setq svg-lib-icons-dir                (var "svg-lib/icons/"))
    (setq sx-cache-directory               (var "sx/cache/"))
    (setq tabspaces-session-file           (var "tabspaces-session.eld"))
    (setq tempel-path                      (etc "tempel/templates.eld"))
    (setq tldr-directory-path              (var "tldr/"))
    (setq transient-history-file           (var "transient/history.el"))
    (setq transient-levels-file            (etc "transient/levels.el"))
    (setq transient-values-file            (etc "transient/values.el"))
    (setq treemacs-last-error-persist-file (var "treemacs/persist-last-error.org"))
    (setq treemacs-persist-file            (var "treemacs/persist.org"))
    (setq undo-fu-session-directory        (var "undo-fu-session/"))
    (setq undohist-directory               (var "undohist/"))
    (setq uptimes-database                 (var "uptimes.el"))
    (setq user-emacs-ensime-directory      (var "ensime/"))
    (setq vimish-fold-dir                  (var "vimish-fold/"))
    (setq wl-address-file                  (etc "wanderlust/address"))
    (setq wl-alias-file                    (etc "wanderlust/alias"))
    (setq wl-folders-file                  (etc "wanderlust/folders"))
    (setq wl-init-file                     (etc "wanderlust/init.el"))
    (setq wl-temporary-file-directory      (var "wanderlust-tmp"))
    (setq wl-x-face-file                   (etc "wanderlust/x-face"))
    (setq x86-lookup-cache-directory       (var "x86-lookup/cache/"))
    (setq xkcd-cache-dir                   (var "xkcd/"))
    (setq yas-snippet-dirs                 (list (etc "yasnippet/snippets/")))

    (eval-after-load 'bbdb        `(make-directory ,(var "bbdb/") t))
    (eval-after-load 'bookmark+-1 `(make-directory ,(var "bmkp/") t))
    (eval-after-load 'command-log-mode '(make-directory clm/logging-dir t))
    (eval-after-load 'company     `(make-directory ,(var "company/") t))
    (eval-after-load 'dape        '(make-directory dape-adapter-dir t))
    (eval-after-load 'elfeed      `(make-directory ,(var "elfeed/") t))
    (eval-after-load 'emojify     '(make-directory emojify-emojis-dir t))
    (eval-after-load 'geiser      `(make-directory ,(var "geiser/") t))
    (eval-after-load 'helm        `(make-directory ,(var "helm/") t))
    (eval-after-load 'jabber      '(make-directory jabber-avatar-cache-directory t))
    (eval-after-load 'jabber      '(make-directory jabber-history-dir t))
    (eval-after-load 'lookup      '(make-directory lookup-init-directory t))
    (eval-after-load 'lsp-mode    `(make-directory ,(var "lsp/") t))
    (eval-after-load 'org-caldav  '(make-directory org-caldav-save-directory t))
    (eval-after-load 'projectile  `(make-directory ,(var "projectile/") t))
    (eval-after-load 'sly         `(make-directory ,(var "sly/") t))
    (eval-after-load 'sx          '(make-directory sx-cache-directory t))
    (eval-after-load 'wl          `(make-directory ,(etc "wanderlust") t))
    (eval-after-load 'xkcd        '(make-directory xkcd-cache-dir t))
    (eval-after-load 'yasnippet   '(make-directory (car yas-snippet-dirs) t))
    ))

;;; Advices

(define-advice emacs-session-filename (:override (session-id) no-littering)
  "Construct a filename to save the session in, based on SESSION-ID.
Unconditionally return a filename in `no-littering-var-directory'."
  (let ((dir (no-littering-expand-var-file-name "emacs-session/")))
    (make-directory dir t)
    (expand-file-name session-id dir)))

;;; Backups

(defvar undo-tree-history-directory-alist)

(defun no-littering-theme-backups ()
  "Theme locations where backups of various sorts are created.

The purpose of this package is to store data files of various
sorts in a handful of central locations, instead of spreading
them all over the place.  When doing that for temporary files,
which contain backups of some sort, that increases the odds that
sensitive data is written to disk in clear text and/or that such
clear text files persist longer, if they would be created anyway.

Because of that, simply loading `no-littering' does not theme
certain, potentially unsafe variables.  Instead, this function is
provided, so that you can decide whether to take the risk or not.

Calling this function sets these variables:
- `auto-save-file-name-transforms' (built-in)
- `backup-directory-alist' (built-in)
- `undo-tree-history-directory-alist' (from `undo-tree')

The default values of these variables cause additional files to
be created in the same directories as the files that are being
visited.  Calling this function changes the values of these
variables, so that this is only done for visited files located in
certain directories.  For all other visited files, the additional
files are created in files inside `no-littering-var-directory'.

Additional files are created in the same directory as the visited
file, for files located in:
- \"/tmp/\"
- \"/dev/shm\"
- `temporary-file-directory'

With these settings it is still possible that sensitive data is
written to additional files, but you are more likely to spot it,
and because these directories usually use a `tmpfs' file-system,
the leaked secrets should not persist after a reboot.

If you do *not* call this function, then these additional files
are always created in the same directory as the visited files,
regardless of the location of the visited files.  In other words,
even when using the default values, there is a significant risk
of leaking sensitive data, and if you want to reduce that, then
you must turn of these features completely."
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
           ,(concat (file-name-as-directory temporary-file-directory) "\\2") t)
          ("\\`/tmp\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
          ("\\`/dev/shm\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
          (".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
          ("\\`/tmp/" . nil)
          ("\\`/dev/shm/" . nil)
          ("." . ,(no-littering-expand-var-file-name "backup/"))))
  (setq undo-tree-history-directory-alist
        `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
          ("\\`/tmp/" . nil)
          ("\\`/dev/shm/" . nil)
          ("." . ,(no-littering-expand-var-file-name "undo-tree-hist/"))))
  )

;;; _
(provide 'no-littering)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; no-littering.el ends here

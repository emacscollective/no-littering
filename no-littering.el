;;; no-littering.el --- help keeping ~/.emacs.d clean

;; Copyright (C) 2016  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: http://github.com/tarsius/no-littering

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
;; variables to put files in either `no-littering-etc-directory'
;; (defaulting to "~/.emacs.d/etc/") or `no-littering-var-directory'
;; (defaulting to "~/.emacs.d/var/"), and by using descriptive file
;; names and subdirectories when appropriate.  This is similar to a
;; color-theme; a "path-theme" if you will.

;; We still have a long way to go until most built-in and many third-
;; party path variables are properly "themed".  Like a color-theme,
;; this package depends on user contributions to accomplish decent
;; coverage.  Pull requests are highly welcome.

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

;; * Files
;;
;; To use an example, the variable `smex-save-file' from the `smex'
;; package is set to `smex-save.el'.  This is based on three
;; principles.  File names should
;;
;; 1. Share the prefix of the package/feature that uses them.
;; 2. Reflect the name of the elisp variable that references them.  If
;;    you only know the name of the file, it should be trivial to find
;;    the variable that references that file in Emacs (e.g. to find
;;    its documentation).

;; 3. Use appropriate extensions (if applicable) to signal the content
;;    of the file and to trigger the correct major-mode.

;; * Directories
;;
;; Any subdirectories at the top-level should use the corresponding
;; package name as a prefix, similar to how files are named.

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

(cl-flet ((etc (file) (expand-file-name (convert-standard-filename file)
                                        no-littering-etc-directory))
          (var (file) (expand-file-name (convert-standard-filename file)
                                        no-littering-var-directory)))
  (with-no-warnings ; many of these variables haven't been defined yet

;;; Built-in packages

    (setq auto-save-list-file-prefix       (var "auto-save-"))
    (setq backup-directory-alist           (list (cons "." (var "backup/"))))
    (setq bookmark-default-file            (var "bookmark-default.el"))
    (setq desktop-path                     (list (var "desktop/")))
    (setq eshell-directory-name            (var "eshell/"))
    (setq ido-save-directory-list-file     (var "ido-save-directory-list.el"))
    (setq nsm-settings-file                (var "nsm-settings.el"))
    (eval-after-load 'org
      `(make-directory ,(var "org/") t))
    (setq org-id-locations-file            (var "org/org-id-locations.el"))
    (setq org-registry-file                (var "org/org-registry.el"))
    (setq recentf-save-file                (var "recentf-save.el"))
    (setq save-place-file                  (var "save-place.el"))
    (setq savehist-file                    (var "savehist.el"))
    (setq tramp-persistency-file-name      (var "tramp-persistency.el"))
    (setq trash-directory                  (var "trash/"))
    (setq url-cache-directory              (var "url/"))
    (setq url-configuration-directory      (etc "url/"))

;;; Third-party packages

    (setq smex-save-file                   (var "smex-save.el"))
    ))

(provide 'no-littering)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; no-littering.el ends here

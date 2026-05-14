(defun no-littering-get-themed-variables ()
  "Return all variables recognized by no-littering."
  (with-temp-buffer
    (insert-file-contents "no-littering.el")
    (goto-char (point-min))
    (re-search-forward "^(cl-letf")
    (beginning-of-line)
    (down-list)
    (forward-sexp 22)
    (let* ((data (cdr (read (current-buffer))))
       (vars (cl-remove-if-not
  	    (lambda (form) (eq (car form) 'setq)) data))
       (current-values
        (mapcar
         (lambda (form)
  	 (let* ((var (cadr form))
  		(value (and (boundp var)
  			    (symbol-value var))))
  	   (when (stringp value)
  	     (setq value (file-truename value)))
  	   (cons var value)))
         vars)))
  current-values)))

(defun no-littering-get-current-variables ()
  "Return all variables recognized by no-littering currently used."
  (cl-remove-if
   (lambda (var) (not (cdr var)))
   (no-littering-get-themed-variables)))

(defun no-littering-generate-migration (old-values new-values)
  (let* ((migrations
      (mapcar (lambda (var)
  	      (list (car var)
  		    (cdr var)
  		    (cdr (assq (car var) new-values))))
  	    old-values))
     (commands
      (mapcar (lambda (var)
  	      (if (stringp (nth 2 var))
  		  (when (not (equal (nth 1 var) (nth 2 var)))
  		    `(progn
  		       ,(format "%s" (car var))
  		       (make-directory
  			,(file-name-directory (nth 2 var))
  			t)
  		       (rename-file ,(nth 1 var) ,(nth 2 var) 1)))
  		`(progn ,(car var) "Value is not a directory")))
  	    migrations)))
    commands))

(defun no-littering--custom-reset-symbol (symbol)
  (put symbol 'variable-comment nil)
  (put symbol 'standard-value nil)
  (put symbol 'customized-value nil)
  (put symbol 'customized-variable-comment nil)
  (when (or (get symbol 'saved-value)
        (get symbol 'saved-variable-comment))
    (put symbol 'saved-value nil)
    (put symbol 'saved-variable-comment nil)))

(defun no-littering-custom-reset ()
  (dolist (var (no-littering-get-themed-variables))
    (no-littering--custom-reset-symbol (car var))))

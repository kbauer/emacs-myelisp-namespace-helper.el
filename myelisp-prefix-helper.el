;;;; TEMPORARY PREFIX HELPERS
;;; Until I implement a proper mode.
;;; 
;;; In emacs-lisp, there is no native namespace system.
;;; Any attempt to implement namespaces as macros is bound
;;; to miss some current or future interactive feature
;;; depending on full function names in the code.
;;; 
;;; This here is meant to make the process less difficult.
;;; 
;;; Usage:
;;; 
;;;   - Paste into your .emacs file
;;;   - In emacs-lisp or lisp-interaction modes, 
;;;     insert a buffer-appropriate namespace prefix 
;;;     by pressing "-" twice. Private functions following
;;;     the "NAMESPACE--NAME" convention are inserted
;;;     by pressing it three times.
;;;   - Works for `M-x eval-expression.' too. Here the
;;;     last used emacs-lisp buffer is assumed.
;;;   - Use file-variable `myelisp-namespace-helper-prefix'
;;;     to override default.


(define-key emacs-lisp-mode-map (kbd "-") #'myelisp-namespace-helper)
(define-key lisp-interaction-mode-map (kbd "-") #'myelisp-namespace-helper)
(define-key read-expression-map (kbd "-") #'myelisp-namespace-helper)


(defvar-local myelisp-namespace-helper-prefix nil
  "Set as file-local variable to override ~ key.")


(put 'myelisp-namespace-helper-prefix 'safe-local-variable #'stringp)


(defun myelisp-namespace-helper ()
  "Following preceding -, Insert NAMESPACE--; Reduce to NAMESPACE- if already there.
With prefix argument, just call `self-insert-command'."
  (interactive)
  (if current-prefix-arg
      (self-insert-command (prefix-numeric-value current-prefix-arg))
    (let (prefix)
      (setq prefix
        (concat (myelisp-namespace-for-buffer) "-"))
      (cond
        ((looking-back 
           (concat "\\_<" (regexp-quote prefix))
           (line-beginning-position))
         (insert "-"))
        ((looking-back "\\_<-" (line-beginning-position))
         (replace-match prefix t t))
        (t
          (self-insert-command 1))))))


(defun myelisp-namespace-for-buffer (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (cond
      ((minibufferp)
       (cl-loop
         for buf being the buffers
         for mode = (with-current-buffer buf major-mode)
         if (memq mode '(emacs-lisp-mode lisp-interaction-mode))
         return (myelisp-namespace-for-buffer buf)
         finally return nil))
      ((stringp myelisp-namespace-helper-prefix)
       myelisp-namespace-helper-prefix)
      ((and myelisp-namespace-helper-prefix
            (symbolp myelisp-namespace-helper-prefix))
       (symbol-name myelisp-namespace-helper-prefix))
      ((buffer-file-name)
       (file-name-base (buffer-file-name)))
      (t
        (replace-regexp-in-string "[[:blank:]]" ""
          (buffer-name))))))

;;;; TEMPORARY PREFIX HELPERS
;; Until I implement a proper mode.
;; Published as https://gist.github.com/kbauer/f629eaf78f8dd1117225eaf2baf717d5
;; 
;; See docstring of `myelisp-prefix-helper'.

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map read-expression-map))
  (define-key map (kbd "-") #'myelisp-namespace-helper))


(defvar-local myelisp-namespace-helper-prefix nil
  "Set as file-local variable to override ~ key.")


(put 'myelisp-namespace-helper-prefix 'safe-local-variable #'stringp)


(defun myelisp-namespace-helper ()
  "Insert - or NAMESPACE-.

In emacs-lisp, there is no native namespace system.
Any attempt to implement namespaces as macros is bound
to miss some current or future interactive feature
depending on full function names in the code.

This here is meant to make the process less difficult.

Usage:

  - Paste into your .emacs file
  
  - In emacs-lisp or lisp-interaction modes, 
    insert a buffer-appropriate namespace prefix 
    by pressing `-' once, while not already inside a symbol.
    
      * Following an already started symbol name, just
        behave like normal `self-insert-command'.
      
      * The same applies inside strings and comments,
        unless `point' is preceded by a backquote (`).
      
      * When invoked with a prefix argument, likewise
        delegate to `self-insert-command'.
    
  - Used `quoted-insert' (normally `C-q -') to insert
    literal hypens.

  - Works for `M-x eval-expression.' too. Here the
    last used emacs-lisp buffer is assumed.

  - Use file-variable `myelisp-namespace-helper-prefix'
    to override default.
  
With this code, `prettify-symbols-mode' collapses the namespace
prefix into a highlighted hyphen, mirroring the input method.

Future plans: Make it configurable, and cross-mode, as a
minor-mode. E.g. in LaTeX, binding @@ to produce 
a package-appropriate prefix would be useful."
  (interactive)
  (let (prefix)
    (setq prefix
      (concat (myelisp-namespace-for-buffer) "-"))
    (cond
      ((and
         ;; If current-prefix-arg is given, assume intent to insert `-' chars.
         (null current-prefix-arg)
         ;; Likewise, when already inside a started symbol.
         (not (looking-back
                (rx (or (syntax word) (syntax symbol))) 
                (1- (point))))
         ;; Inside strings and comments, only insert prefix when
         ;; preceded by backquote (`). Strings and comments are recognized
         ;; in font-lock-mode by *any* face being set.
         (or (null (face-at-point))
             (looking-back "`" (1- (point)))))
       (insert prefix))
      (t
        ;; inherits `current-prefix-arg'.
        (call-interactively #'self-insert-command)))))


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


(mymessage "Enabling support for prefix abbreviation with `prettify-symbols-mode'.")


(add-hook 'prettify-symbols-mode-hook #'myelisp-namespace-helper-pretty-init)
(defun myelisp-namespace-helper-pretty-init ()
  "Support for displaying prefices in `prettify-symbols-mode'."
  (let*((prefix-regexp
          (concat
            "\\_<"
            (regexp-quote (myelisp-namespace-for-buffer))
            "\\(?1:--?\\)")))
    (with-temp-message "Enabling reduction of namespace prefix.")
    (make-local-variable 'font-lock-extra-managed-props)
    (push 'display font-lock-extra-managed-props)
    (make-local-variable 'prettify-symbols-alist)
    (remove-text-properties (point-min) (point-max) '(display))
    (font-lock-add-keywords nil
      `((,prefix-regexp 1 (myelisp-namespace-helper-pretty-apply) prepend)))))


(defun myelisp-namespace-helper-pretty-apply ()
  (prog1 nil
    (when prettify-symbols-mode
      (add-text-properties
        (match-beginning 0)
        (match-end 0)
        (list 'display 
          (propertize
            (substring-no-properties (match-string 1))
            'face 'myelisp-namespace-helper-pretty-face))))))


(defface myelisp-namespace-helper-pretty-face 
  '((t :inherit (bold link)))
  "Face used for highlighting collapsed namespace prefices."
  :group 'myelisp)
;;; myelisp-prefix-helper.el --- A hack for namespace insertion  -*- lexical-binding: t; coding: utf-8; lisp-indent-offset: nil; -*-

;; Copyright (C) 2018  Klaus-Dieter Bauer

;; Author: Klaus-Dieter Bauer <kdb.devel@gmail.com>
;; Keywords: lisp, tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;;; TEMPORARY PREFIX HELPERS
;; Until I implement a proper mode.
;; Published as `https://gist.github.com/kbauer/f629eaf78f8dd1117225eaf2baf717d5'
;; 
;; See docstring of `myelisp-prefix-helper'.

;;; Code:


(require 'cl-macs)


(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map read-expression-map))
  (define-key map (kbd "-") #'myelisp-namespace-helper))


(defvar-local myelisp-namespace-helper-prefix nil
  "Set as file-local variable to override ~ key.")


(put 'myelisp-namespace-helper-prefix 'safe-local-variable #'stringp)


(defun myelisp-namespace-helper ()
  "Insert - or NAMESPACE-.


USAGE
=====

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


MOTIVATION
==========

In emacs-lisp, there is no native namespace system. While it has
the convenient consequence, that the same symbol means the same
everywhere (a boon for interactive features like `M-x
describe-function' or `M-x find-function'), it also results in a
lot of “boilerplate” re-typing of namespace prefix strings.

While implementations of macro-based namespaces such as
`names.el' exist, they are bound to lack support for current or
future interactive features, that depend on the presence of full
function names in the source-code files.

Any attempt to implement namespaces as macros is bound
to miss some current or future interactive feature
depending on full function names in the code.

This provisional package is meant to make working with namespace
prefices more convenient in the meantime.

It is also meant for experimenting with what works.


TODO: WHAT WORKS?
=================

  - Prettification, and maybe insertion, should be limited to
    `private--names' by default. The programmer should remain
    aware of how verbose client code will see the “public”
    interface.

  - This would also solve the issue of typing negative numbers
    or the `-' function without `C-q' prefix.


WHAT DID NOT WORK
=================

  - Key `~'. It is the least common printable ASCII character
    in Emacs's lisp code, but using the same key for inserting
    the prefix and for separating words in symbols proved a huge
    usability advantage.

  - Double `--' for prefix, triple `---' for private prefix.
    Possibly a matter of training, but it felt wrong.


TODO: FUTURE PLANS
==================

Make it configurable, and cross-mode, as a minor-mode. E.g. in
LaTeX, binding @@ to produce a package-appropriate prefix would
be useful."
  (interactive)
  (let (prefix)
    (setq prefix
      (concat (myelisp-namespace-helper-get-buffer-namespace) "-"))
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


(defun myelisp-namespace-helper-get-buffer-namespace (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (cond
      ((minibufferp)
       (cl-loop
         for buf being the buffers
         for mode = (with-current-buffer buf major-mode)
         if (memq mode '(emacs-lisp-mode lisp-interaction-mode))
         return (myelisp-namespace-helper-get-buffer-namespace buf)
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


(message "myelisp-prefix-helper: Enabling support for prefix abbreviation with `prettify-symbols-mode'.")


(add-hook 'prettify-symbols-mode-hook #'myelisp-namespace-helper-pretty-init)
(defun myelisp-namespace-helper-pretty-init ()
  "Support for displaying prefices in `prettify-symbols-mode'."
  (let*((prefix-regexp
          (concat
            "\\_<"
            (regexp-quote (myelisp-namespace-helper-get-buffer-namespace))
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


(provide 'myelisp-namespace-helper)
;;; myelisp-prefix-helper.el ends here

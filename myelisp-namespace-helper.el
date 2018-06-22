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
;; Currently published as `https://github.com/kbauer/myelisp-namespace-helper.el'
;; Previously published as `https://gist.github.com/kbauer/f629eaf78f8dd1117225eaf2baf717d5'
;; 
;; See docstring of `myelisp-prefix-helper'.

;;; Code:


(require 'cl-macs)


(defvar-local myelisp-namespace-helper-prefix nil
  "When set as a file-local variable, overrides the default namespace prefix.
Expected to be a symbol.")
(put 'myelisp-namespace-helper-prefix 'safe-local-variable #'symbolp)


(defvar-local myelisp-namespace-helper--function nil
  "The `post-self-insert-hook' function used in the current buffer.")


(defvar-local myelisp-namespace-helper-char ?-
  "Character to treat as prefix character.

Determined by either
  - `myelisp-namespace-helper-affected-modes-alist'
  - Its value as file-variable (must be a character).
  - or ?- by default.")
(put 'myelisp-namespace-helper-char 'safe-local-variable #'characterp)


(defconst myelisp-namespace-helper-inhibiting-faces
  (list
    'font-lock-comment-face
    'font-lock-comment-delimiter-face
    'font-lock-doc-face
    'font-lock-string-face)
  "A list of faces, that inhibit special behavior of the `-' key,
unless the preceding context marks the intent of inserting a symbol.

See `myelisp-namespace-helper--function' for details.")


(defcustom myelisp-namespace-helper-affected-modes-alist
  (list
    (list 'emacs-lisp-mode       ?- t)
    (list 'lisp-interaction-mode ?- t)
    (list 'tex-mode              ?@ nil)
    (list 'latex-mode            ?@ nil))
  "Alist configuring modes affected by `myelisp-namespace-helper-mode'.

Each entry has the form (MAJOR-MODE CHAR ENABLED) where

MAJOR-MODE is a symbol denoting a major-mode,
CHAR is the character triggering namespace insertion
  in that mode, and namespace collapsing with
  `prettify-symbols-mode', and
ENABLED is a boolean flag, determining whether
  `myelisp-namespace-helper-global-mode' affects
  this mode."
  :group 'myelisp-namespace-helper
  :type 
  `(repeat 
     (list (symbol :tag "Major mode")
           (character :tag "Trigger character")
           (boolean :tag "Affected by global mode"))))


(eval-and-compile
(defun myelisp-namespace-helper--function-for-char (char &optional noerror)
  "Return the `post-self-insert-hook' function associated with `CHAR'.
If it hasn't been defined, signal error."
  (let ((name (intern (format "myelisp-namespace-helper--function-for/%c" char))))
    (if (or noerror (fboundp name))
        name
      (error 
          (concat "Namespace function for `%c' hasn't been defined "
            "with `myelisp-namespace-helper-define-function-for-char'.")
        char)))))


(defmacro myelisp-namespace-helper-define-function-for-char (char)
  (let ((string (string char)))
   `(defun ,(myelisp-namespace-helper--function-for-char char t) ()
      (cond
        ((and (eq last-input-event ,char)
              
              ;; Insert prefix, if looking at lone `-' character.
              (looking-back (rx symbol-start ,string) (1- (point)))
              
              ;; If face indicates comment or string, only replace `-'
              ;; character if preceded by quote character indicating
              ;; that a symbol name is meant.
              (or (not (memq (face-at-point) myelisp-namespace-helper-inhibiting-faces))
                  (looking-back (rx (any "`'") ,string) (- (point) 2))))
         (backward-delete-char 1)
         (unless (minibufferp)
           (with-temp-message ,(concat "`myelisp-namespace-helper-mode': Replacing `"string"' by prefix.")))
         (insert (myelisp-namespace-helper-get-buffer-namespace) ,string))
        
        ;; After number or whitespace keys, replace the prefix by a sole hyphen,
        ;; as it would otherwise interfere with typing `-1' or `(- 1 2)'.
        ((and (memq last-input-event '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\ ?\n return))
              (eq ,char (char-before (1- (point))))
              (save-excursion
                (backward-char)
                (looking-back
                  (concat "\\_<" 
                    (regexp-quote
                      (concat
                        (myelisp-namespace-helper-get-buffer-namespace) ,string)))
                  (line-beginning-position))))
         (unless (minibufferp)
           (with-temp-message ,(concat "`myelisp-namespace-helper-mode': Replacing prefix by `"string"'.")))
         (save-excursion (replace-match ,string t t)))))))


(myelisp-namespace-helper-define-function-for-char ?-)
(myelisp-namespace-helper-define-function-for-char ?@)


(define-minor-mode myelisp-namespace-helper-mode 
  "Use the `-' key to insert a namespace prefix in `emacs-lisp-mode'."
  :lighter " Nsh"

  (when myelisp-namespace-helper-mode
    (setq-local myelisp-namespace-helper-char nil)
    (hack-local-variables)
    (setq-local myelisp-namespace-helper-char
      (or myelisp-namespace-helper-char
          (nth 1 (assq major-mode myelisp-namespace-helper-affected-modes-alist))
          ?-))
    (setq-local myelisp-namespace-helper--function
      (myelisp-namespace-helper--function-for-char myelisp-namespace-helper-char))
    (add-hook 'post-self-insert-hook myelisp-namespace-helper--function nil t))
  (unless myelisp-namespace-helper-mode
    (remove-hook 'post-self-insert-hook myelisp-namespace-helper--function t)))


(define-global-minor-mode myelisp-namespace-helper-global-mode myelisp-namespace-helper-mode
  (lambda ()
    (if (nth 2 (assq major-mode myelisp-namespace-helper-affected-modes-alist))
        (myelisp-namespace-helper-mode +1)
      (myelisp-namespace-helper-mode -1))))

;; Ensure that the global mode is actually enabled by the customization variable.
(when myelisp-namespace-helper-global-mode
  (myelisp-namespace-helper-global-mode +1))


(defun myelisp-namespace-helper-get-buffer-namespace (&optional buf)
  "Returns namespace prefix as string, without trailing `-'.

Usually the `buffer-file-name' or `buffer-name', but can be
overriden by the `myelisp-namespace-helper-prefix' file-local variable."
  (with-current-buffer (or buf (current-buffer))
    (when (stringp myelisp-namespace-helper-prefix)
      (setq-local myelisp-namespace-helper-prefix (intern myelisp-namespace-helper-prefix)))
    (cond
      ((minibufferp)
       (cl-loop
         for buf being the buffers
         for mode = (with-current-buffer buf major-mode)
         if (memq mode '(emacs-lisp-mode lisp-interaction-mode))
         return (myelisp-namespace-helper-get-buffer-namespace buf)
         finally return nil))
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
            "\\(?1:"
            (regexp-quote 
              (string myelisp-namespace-helper-char myelisp-namespace-helper-char))
            "?\\)")))
    (with-temp-message "Enabling reduction of namespace prefix.")
    (make-local-variable 'font-lock-extra-managed-props)
    (push 'display font-lock-extra-managed-props)
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

;;; -*-Emacs-Lisp-*-
;;; scala-mode-fontlock.el -

;; Copyright (C) 2009-2011 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop

;;; License

;; SCALA LICENSE
;;
;; Copyright (c) 2002-2011 EPFL, Lausanne, unless otherwise specified.
;; All rights reserved.
;;
;; This software was developed by the Programming Methods Laboratory of the
;; Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;
;; Permission to use, copy, modify, and distribute this software in source
;; or binary form for any purpose with or without fee is hereby granted,
;; provided that the following conditions are met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;;    3. Neither the name of the EPFL nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;
;;
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'scala-mode-fontlock)

(require 'cl)
(require 'font-lock)
(require 'scala-mode-constants)
(require 'scala-mode-lib)
(require 'scala-mode-navigation)

(defcustom scala-mode-fontlock:multiline-highlight t
  "Non-nil means enable multiple line highlight support, which
may cause emacs slow down in certain condition. Set this variable
to nil if you want to disable multiple line highlight support."
  :type 'boolean
  :group 'scala)

(defun scala-mark-borders (funs)
  (loop for (fun . flag) in funs
        do (scala-forward-ignorable)
        if flag collect (point-marker)
        while (funcall fun)
        if flag collect (point-marker)))

(defun scala-make-match (funs)
  (let ((start-mark (point-marker))
        (markers (scala-mark-borders funs))
        (end-mark (point-marker)))
    (cons start-mark (cons end-mark markers))))

(defconst scala-binding-end-re
  (regexp-opt '("=" "=>" ";" "<-")))

(defconst scala-font-lock-limit-re
  (concat scala-binding-end-re "\\|" scala-keywords-re))


(defun scala-font-lock-limit ()
  "Find font lock limit and mark multiple line construct in
current context."
  (let ((p0 (point))
        (p1 (line-end-position)))
    ;; multiple line construct will only start with '[' or '(', not '{'
    (scala-forward-ignorable)
    (when (looking-at "[\\[(]")
      (save-excursion
        ;; skip all parameter groups in "def foo(a: Int)(b: Int)"
        (while (progn (save-excursion (scala-forward-ignorable) (looking-at "[\\[(]")))
          (scala-forward-ignorable)
          ;; forward the list, but if that's not possible, to the first empty line, 
          ;; end of block, or end of buffer (which ever is first)
          (let ((pos (point)))
            (ignore-errors (forward-list))
            (when (or (= (char-before) ?\})
                      (= pos (point)))
              (let ((limit (if (= pos (point)) (buffer-size) (point))))
                (goto-char pos)
                (unless (search-forward-regexp scala-empty-line-re limit t)
                  (end-of-buffer))))))
        ;; if we ended at the end of ')' or ']', there might be more
        ;; coming, include next line (if empty) to the limit
        (when (and (eq (char-syntax (char-before)) ?\))
                   (progn (scala-forward-ignorable) (scala-looking-at-empty-line)))
          (search-forward-regexp scala-empty-line-re nil t))
        (setq p1 (point)))
      (when scala-mode-fontlock:multiline-highlight
        (put-text-property p0 p1 'font-lock-multiline t)))
    p1))


(defun scala-match-and-skip-binding (limit)
  (scala-forward-ignorable)
  (skip-chars-forward ",(")
  (scala-forward-ignorable)
  (and (not (or (looking-at "\\<\\(extends\\|with\\)\\>\\|{")
                (= (char-after) ?:)
                (scala-looking-at-special-identifier scala-binding-end-re)))
       (ignore-errors
         (save-restriction
           (narrow-to-region (point-min) limit)
           (let ((matches (scala-make-match
                           '((scala-forward-ident . t)
                             ((lambda ()
                                (when (= (char-after) ?:)
                                  (forward-char)
                                  t)) . nil)
                             ((lambda ()
                                (scala-forward-type)
                                (scala-when-looking-at "\\s *\\*")
                                t) . t)))))
             (scala-forward-ignorable)
             ;; forward parameter default value
             (when (= (char-after) ?=)
               (skip-syntax-forward "^,)")
               )
             (skip-chars-forward ",)")
             (set-match-data matches)))
         t)))

(defun scala-match-and-skip-ident (limit)
  (scala-forward-ignorable)
  (when (and (not (looking-at scala-keywords-re))
             (looking-at scala-qual-ident-re))
    (goto-char (match-end 0))
    t))

(defun scala-match-and-skip-type-param (limit)
  (scala-forward-ignorable)
  (scala-when-looking-at "[[,]"
    (let ((matches (scala-make-match '((scala-forward-type-param . t)))))
      (while (progn (scala-forward-ignorable) (scala-when-looking-at "\\]")))
      (set-match-data matches)
      t)))

(defun scala-match-and-skip-result-type (limit)
  (scala-forward-ignorable)
  (scala-when-looking-at ":"
    (scala-forward-ignorable)
    (set-match-data (list (point-marker)
                          (progn (scala-forward-type) (point-marker))))
    t))

(defconst scala-pattern-end-re
  (regexp-opt '("if" "case" "class") 'words))

(defconst scala-pattern-end-special-re
  (regexp-opt '( "=>" "=" "<-") t))

(defun scala-match-and-skip-pattern (limit)
  (scala-forward-ignorable)
  (while (progn
           (skip-chars-forward "()[], ")
           (and (not (or (looking-at scala-pattern-end-re)
                         (scala-looking-at-special-identifier
                          scala-pattern-end-special-re)))
                (looking-at scala-literal-re)))
    (goto-char (match-end 0)))
  (and (not (or (looking-at scala-pattern-end-re)
                (scala-looking-at-special-identifier scala-pattern-end-special-re)))
       (let ((case-fold-search nil))
         (cond ((looking-at scala-capitalized-ident-re)
                (goto-char (match-end 0)))
               ((scala-match-and-skip-binding limit) t)))))


(defvar scala-font-lock-keywords
  `(;; keywords
    (,scala-keywords-re 0 font-lock-keyword-face nil)

    ;; annotations
    (,scala-annotation-re 0 font-lock-preprocessor-face nil)

    ;; symbols
    (,scala-symbol-re 0 font-lock-string-face nil)

    ;; chars
    (,scala-char-re 0 font-lock-string-face nil)

    ;; constants
    (,scala-constants-re
     0 ,(if (boundp 'font-lock-constant-face)
	    'font-lock-constant-face
	  'font-lock-keyword-face)
     nil)

    ;; modules
    (,(concat "\\<\\(module\\|object\\)\\>\\s *\\(" scala-ident-re "\\)")
     (2 font-lock-variable-name-face nil))

    ;; type definitions
    (,(concat "\\<type\\>\\s *\\(" scala-ident-re "\\)")
     (1 font-lock-type-face nil))

    ;; variables
    ("\\<var\\>"
     (scala-match-and-skip-binding nil nil
				   (1 font-lock-variable-name-face nil)
				   (2 font-lock-type-face nil t)))

    ;; functions
    (,(concat "\\(^\\|[^(,]\\)\\s *\\<def\\>"
	      "\\s *"
	      "\\("
	      scala-ident-re
	      "\\)")
     (2 font-lock-function-name-face nil)
     (scala-match-and-skip-type-param (scala-font-lock-limit) nil
				      (1 font-lock-type-face nil t))
     (scala-match-and-skip-binding (scala-font-lock-limit) nil
				   (1 font-lock-variable-name-face nil)
				   (2 font-lock-type-face nil t))
     (scala-match-and-skip-result-type (scala-font-lock-limit) nil
				       (0 font-lock-type-face nil)))

    ;; class definitions
    ("\\<\\(class\\|trait\\)\\>"
     (scala-match-and-skip-ident nil nil
				 (1 font-lock-type-face nil))
     (scala-match-and-skip-type-param (scala-font-lock-limit) nil
				      (1 font-lock-type-face nil t))
     (scala-match-and-skip-binding (scala-font-lock-limit) nil
				   (1 font-lock-variable-name-face nil)
				   (2 font-lock-type-face nil t)))

    ;; "extends" and "with" clauses
    ("\\<\\(extends\\|with\\)\\>\\s *[^{]"
     (scala-match-and-skip-ident (goto-char (1- (match-end 0))) nil
				 (0 font-lock-type-face nil))
     (scala-match-and-skip-type-param (scala-font-lock-limit) nil
				      (1 font-lock-type-face nil t)))

    ;; patterns
    ("\\<\\(case\\|val\\)\\>"
     (scala-match-and-skip-pattern nil nil
				   (1 font-lock-variable-name-face nil)
				   (2 font-lock-type-face nil t)))
    ))


(defvar scala-font-lock-syntactic-keywords 
  ;; highligh '"' correctly
  `(("'\"'" (0 "\"" t nil))))

;;; modelyze-mode.el
;;; derived from mcode-mode.el, design by Elias Castegren

;;;;;;;;;;;;;;;;;;
;; Highlighting ;;
;;;;;;;;;;;;;;;;;;

;; Please keep this list sorted
(setq modelyze-keywords
     '(
       "def"
       "match"
       "with"
       "if"
       "then"
       "else"
       "type"
       "include"
       "peval"
       "fun"
       "lift"
       "sym"
       "sval"
       "error"
      ))

(setq modelyze-constants
      '(
        "false"
        "true"
        ))

(setq modelyze-primitives
      '( )) ;; Primitive types (intensionally left blank)

(setq modelyze-operators
     '( )) ;; Intensionally left blank

(setq modelyze-warning
     '("include"
       ))

(setq modelyze-keywords-regexp (regexp-opt modelyze-keywords 'symbols))
(setq modelyze-operators-regexp (regexp-opt modelyze-operators 'symbols))
(setq modelyze-constants-regexp (regexp-opt modelyze-constants 'symbols))
(setq modelyze-primitives-regexp (regexp-opt modelyze-primitives 'symbols))
(setq modelyze-warning-regexp (regexp-opt modelyze-warning 'symbols))

(setq modelyze-types-regexp "\\_<[[:upper:]][[:word:]]*\\_>")

(setq modelyze-font-lock-keywords
     `(
       (,modelyze-keywords-regexp   . font-lock-keyword-face)
       (,modelyze-constants-regexp  . font-lock-constant-face)
       (,modelyze-primitives-regexp . font-lock-type-face)
       (,modelyze-operators-regexp  . font-lock-builtin-face)
       (,modelyze-types-regexp      . font-lock-type-face)
       (,modelyze-warning-regexp     . font-lock-warning-face)
       )
     )

(defvar modelyze-mode-syntax-table nil "Syntax table for `modelyze-mode'.")

(setq modelyze-mode-syntax-table
     (let ( (synTable (make-syntax-table)))
       ;; Inline comment “// ...”
       ;; Inline comment “-- ...”
       (modify-syntax-entry ?/ ". 12a" synTable)
       ;;(modify-syntax-entry ?- "_ 123" synTable)
       (modify-syntax-entry ?\n ">" synTable)
       synTable))

;;;;;;;;;;;;;;
;; prettify ;;
;;;;;;;;;;;;;;

(defvar modelyze-prettify-symbols-alist
  '(("fun" . ?λ))
  "List of syntax to prettify for `modelyze-mode'.")

(if (boundp 'prettify-symbols-alist)
    (add-hook 'modelyze-mode-hook
              (lambda ()
                (mapc (lambda (pair) (push pair prettify-symbols-alist))
                      modelyze-prettify-symbols-alist))))

;;;;;;;;;;;;;;;;;
;; compilation ;;
;;;;;;;;;;;;;;;;;

(add-hook 'modelyze-mode-hook
          (lambda ()
            ;; Set default compile command
            (progn
              (set (make-local-variable 'compile-command)
                   (concat "miking " (buffer-name)))
              ;; Get location of standard library from environment
              (let ((path
                     (replace-regexp-in-string
                      "[[:space:]\n]*$" ""
                      (shell-command-to-string "$SHELL -l -c 'echo $MODELYZE_STDLIB'"))))
                (if (> (length path) 0)
                  (set (make-local-variable 'compilation-environment)
                       (list (concat "MODELYZE_STDLIB=" path))))))))

(setq modelyze-error-regexp
      '(modelyze "\"\\(.+\\)\" \\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
(add-hook 'compilation-mode-hook
          (lambda ()
            (add-to-list 'compilation-error-regexp-alist-alist modelyze-error-regexp)
            (add-to-list 'compilation-error-regexp-alist 'modelyze)))

;;;;;;;;;;;;;;;;;;;;;
;; mode definition ;;
;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode modelyze-mode prog-mode
 (setq font-lock-defaults '(modelyze-font-lock-keywords))
 (setq mode-name "modelyze")
 )

;; Open “*.modelyze” in modelyze-mode
(add-to-list 'auto-mode-alist '("\\.moz\\'" . modelyze-mode))

(provide 'modelyze-mode)
;;; modelyze-mode.el ends here

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Tom Fryers"
      user-mail-address "tom.fryers@midsummerenergy.co.uk")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Source Code Pro" :size 15))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! doom-modeline
  (setq doom-modeline-check-simple-format t)
  (setq doom-modeline-modal nil)
  (setq doom-modeline-percent-position '(-3 "%o")))

(setq calc-angle-mode 'rad
      calc-symbolic-mode t)

(after! corfu
  (setq corfu-auto nil)
  (setq corfu-preview-current nil)
  (setq tab-always-indent nil))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-banner)

(setq lsp-rust-analyzer-cargo-watch-command "clippy")

(setq sentence-end-double-space nil)

(evil-define-operator rust-wrap (beg end)
  "Wrap, taking indentation into account."
  (let ((old-fill-column fill-column))
    (setq fill-column (min (+ (current-indentation) 80) 100))
    (evil-fill beg end)
    (setq fill-column old-fill-column)))

(setq auth-sources '("~/.authinfo"))

(setq +format-on-save-disabled-modes
      '(emacs-lisp-mode sql-mode tex-mode latex-mode org-msg-edit-mode rjsx-mode typescript-mode))

(use-package dap-mode
  :defer t
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  ;; (require 'dap-lldb)
  (require 'dap-cpptools)
  ;; (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  ;; (dap-gdb-lldb-setup)
  (dap-cpptools-setup)
  (dap-register-debug-template
   "Rust::CppTools Run Configuration"
   (list :type "cppdbg"
         :request "launch"
         :name "Rust::Run"
         :MIMode "gdb"
         :miDebuggerPath "rust-gdb"
         :environment []
         :program "${workspaceFolder}/target/debug/REPLACETHIS"
         :cwd "${workspaceFolder}"
         :console "external"
         :dap-compilation "cargo build"
         :dap-compilation-dir "${workspaceFolder}")))

(with-eval-after-load 'dap-mode
  ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (setq dap-default-terminal-kind "integrated")
  (dap-auto-configure-mode +1))

(evil-define-operator rust-format-expression (beg end)
  "Call rustfmt on an expression."
  (let ((n (/ (current-indentation) 4)))
    (forward-line 0)
    (let ((start (point)))
      (dotimes (_ n) (insert "fn f(){"))
      (insert "\n")
      (evil-visual-goto-end)
      (forward-line 1)
      (dotimes (_ n) (insert "}"))
      (insert "\n")
      (+format-region
       start (point)
       (lambda ()
         (forward-line (- n))
         (dotimes (_ n)
           (delete-line))
         (goto-char start)
         (dotimes (_ n)
           (delete-line)))))))

(defun insert-html-variable ()
  "Insert a variable inside an HTML tag."
  (interactive)
  (let ((name (read-from-minibuffer "<")))
    (insert "<var>" name "</var>")))

(map! "C-," #'insert-html-variable)

(setq code-review-auth-login-marker 'forge)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(after! apheleia
  (setf (alist-get 'typescript-mode apheleia-mode-alist) '(js-beautify))
  (setf (alist-get 'rjsx-mode apheleia-mode-alist) '(js-beautify))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff)))

(setq treesit-extra-load-path '("~/tree-sitter-module/dist"))

(setq lsp-pylsp-plugins-ruff-enabled t
      lsp-pylsp-plugins-mypy-enabled t)

(undefadvice! +evil--no-squeeze-on-fill-a (fn &rest args)
  :around '(evil-fill evil-fill-and-move))

(after! lsp-mode
  (setq lsp-rust-analyzer-completion-postfix-enable nil)
  (setq lsp-rust-analyzer-completion-add-call-argument-snippets nil)
  (setq lsp-lens-enable nil))

(after! rustic
  (map! :map rustic-mode-map "<normal-state> g w" #'rust-wrap))

(after! code-review
  (setq code-review-lgtm-message "This all looks OK."))

(setq ispell-dictionary "en_GB")
(setq frame-title-format "%b – Emacs")

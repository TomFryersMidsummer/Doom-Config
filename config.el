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

(setq evil-escape-key-sequence "jk")

(after! doom-modeline
  (setq doom-modeline-check-simple-format t)
  (setq doom-modeline-modal nil)
  (setq doom-modeline-battery nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq doom-modeline-percent-position '(-3 "%o")))

(setq calc-angle-mode 'rad
      calc-symbolic-mode t)

(after! corfu
  (setq corfu-auto nil)
  (setq corfu-preview-current nil)
  (setq tab-always-indent nil))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-banner)

(setq sentence-end-double-space nil)

(defun wrap-to (beg end width)
  "Wrap a block of text to a given width."
  (let ((old-fill-column fill-column))
    (setq fill-column width)
    (evil-fill beg end)
    (setq fill-column old-fill-column)))

(evil-define-operator rust-wrap (beg end)
  "Wrap Rust code, taking indentation into account."
  (wrap-to beg end (min (+ (current-indentation) 80) 100)))

(evil-define-operator python-wrap (beg end)
  "Wrap long blocks of text to 72 characters, as per PEP 8."
  (wrap-to beg end 72))

(setq auth-sources '("~/.authinfo"))

(setq +format-on-save-disabled-modes
      '(emacs-lisp-mode sql-mode tex-mode latex-mode
        org-msg-edit-mode))

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
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(after! apheleia
  (push '(php-cs-fixer . ("php-auto-formatter" filepath))
        apheleia-formatters)
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff))
  (setf (alist-get 'php-mode apheleia-mode-alist) '(php-cs-fixer)))

(map! :leader :desc "Save buffer" "f s" #'save-buffer)

(setq treesit-extra-load-path '("~/tree-sitter-module/dist"))

(setq lsp-pylsp-plugins-ruff-enabled t
      lsp-pylsp-plugins-mypy-enabled t)

(undefadvice! +evil--no-squeeze-on-fill-a (fn &rest args)
  :around '(evil-fill evil-fill-and-move))

(after! lsp-mode
  (setq lsp-rust-analyzer-completion-postfix-enable nil)
  (setq lsp-rust-analyzer-completion-add-call-argument-snippets nil)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-import-prefix "self")
  (setq lsp-rust-analyzer-import-granularity "module")
  (lsp-register-custom-settings
   '(("rust-analyzer.completion.termSearch.enable" t t)
     ("rust-analyzer.assist.preferSelf" t t)))
  (setq lsp-lens-enable nil)
  (setq lsp-auto-execute-action nil))

(map! :map python-mode-map "<normal-state> g w" #'python-wrap)

(after! rustic
  (map! :map rustic-mode-map "<normal-state> g w" #'rust-wrap)
  (setq rustic-default-clippy-arguments "--all-targets --all-features --workspace")
  (setq rustic-cargo-check-arguments "--all-targets --all-features --workspace"))

(after! code-review
  (setq code-review-lgtm-message "This all looks OK."))

(setq ispell-dictionary "en_GB")
(setq frame-title-format "%b – Emacs")

(evil-ex-define-cmd "prr" #'pr-review)
(evil-ex-define-cmd "prs" #'pr-review-search)
(evil-ex-define-cmd "prn" #'pr-review-notification)
(add-to-list 'browse-url-default-handlers
             '(pr-review-url-parse . pr-review-open-url))
(add-to-list 'auto-mode-alist
             '("/\\(?:Cargo\\|uv\\)\\.lock\\'" . conf-toml-mode))
(add-to-list 'auto-mode-alist
             '("\\.Dockerfile\\'" . dockerfile-mode))

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(setq! evil-want-Y-yank-to-eol nil)

(add-hook 'python-base-mode-hook 'pet-mode -10)

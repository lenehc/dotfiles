;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:

(setq doom-font                 (font-spec :family "MesloLGS Nerd Font" :size 12 :weight 'regular)
      doom-variable-pitch-font  (font-spec :family "SF Pro Display" :size 14 :weight 'medium))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;;(add-hook 'org-mode-hook (lambda () (variable-pitch-mode) (display-line-numbers-mode -1)))
;;(add-hook 'org-mode-hook #'auto-fill-mode)

(defun org-settings ()
  (visual-line-fill-column-mode)
  (setq visual-fill-column-width 90)
;;  (setq visual-fill-column-center-text t)
;;  (display-line-numbers-mode -1)
  )

(add-hook 'org-mode-hook 'org-settings)
;; (add-hook 'org-mode-hook #'visual-line-fill-column-mode)
;; (setq visual-fill-column-width 100)
;; (setq visual-fill-column-center-text t)

(require 'ol)
(org-link-set-parameters "zotero" :follow
                         (lambda (zpath)
                           (browse-url
                             (format "zotero:%s" zpath))))

;; (custom-theme-set-faces
;;   'user
;;   '(org-block                  ((t (:inherit fixed-pitch))))
;;   '(linum                      ((t (:inherit fixed-pitch))))
;;   '(org-code                   ((t (:inherit (shadow fixed-pitch)))))
;;   '(org-document-info          ((t (:foreground "dark orange"))))
;;   '(org-document-info-keyword  ((t (:inherit (shadow fixed-pitch)))))
;;   '(org-indent                 ((t (:inherit (org-hide fixed-pitch)))))
;;   '(org-link                   ((t (:foreground "royal blue" :underline t))))
;;   '(org-meta-line              ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;   '(org-property-value         ((t (:inherit fixed-pitch))) t)
;;   '(org-special-keyword        ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;   '(org-table                  ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;   '(org-tag                    ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;   '(org-verbatim               ((t (:inherit (shadow fixed-pitch))))))
;;
;; (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;
;;   (custom-theme-set-faces 'user
;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2))))
;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; Enable indent mode by default
(setq org-startup-indented t)

;; Use ';' to enter comand mode in evil
(define-key evil-motion-state-map ";" #'evil-ex)

;; Don't display exit confirmation
(setq confirm-kill-emacs nil)

;; Org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

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

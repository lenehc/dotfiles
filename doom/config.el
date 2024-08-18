;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font                 (font-spec :family "MesloLGS Nerd Font" :size 12 :weight 'regular)
      doom-variable-pitch-font  (font-spec :family "Georgia" :size 14 :weight 'medium))
(setq doom-theme 'doom-gruvbox)
(setq display-line-numbers-type t)
(setq confirm-kill-emacs nil)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(require 'ol)
(org-link-set-parameters "zotero" :follow
                         (lambda (zpath)
                           (browse-url
                             (format "zotero:%s" zpath))))


(defun org-settings ()
  (visual-line-fill-column-mode)
  (setq visual-fill-column-width 90)
  (setq visual-fill-column-center-text t)
  (setq org-startup-indented t)
  (display-line-numbers-mode -1))
(add-hook 'org-mode-hook 'org-settings)

(setq org-directory "~/notes/org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/notes/org/mobileorg/inbox.org")
(setq org-roam-directory "~/notes/roam/")
(setq org-roam-dailies-directory "journal/")

(use-package! org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n d" . org-id-get-create)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      '(("m" "main" plain "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n\n")
         :unnarrowed t
         :empty-lines 1)
        ("d" "default" plain "%?"
         :if-new (file+head "%<%Y-%m-%d %H.%M.%S>-${slug}.org"
                            "#+title: ${title}\n\n")
         :unnarrowed t
         :empty-lines 1)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y>/%<%m>/%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n\n")
         :unnarrowed t
         :empty-lines 1)))

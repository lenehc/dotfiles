;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(if (equal (system-name) "CHECHE")
    (setq doom-font                 (font-spec :family "MesloLGS Nerd Font" :size 14 :weight 'regular)
          doom-variable-pitch-font  (font-spec :family "Georgia" :size 14 :weight 'medium))
  (setq doom-font                 (font-spec :family "MesloLGS Nerd Font" :size 12 :weight 'regular)
        doom-variable-pitch-font  (font-spec :family "Georgia" :size 14 :weight 'medium)))
(setq doom-theme 'doom-gruvbox)
(setq display-line-numbers-type t)
(setq confirm-kill-emacs nil)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(require 'ol)
(org-link-set-parameters "zotero" :follow
                         (lambda (zpath)
                           (browse-url
                             (format "zotero:%s" zpath))))

(setq send-to-mobile-directory "~/Koofr/org")

(defun send-to-mobile ()
  "Export current buffer as PDF using Pandoc and send to mobile directory."
  (interactive)
  (let ((filename
        (buffer-file-name))
        (basename
        (file-name-nondirectory (buffer-file-name))))
    (message "Processing current file...")
    (if (and (string-match-p
                (regexp-quote ".org") basename)
               (not (string-match-p
                     (regexp-quote "[") basename)))
    (shell-command
     (concat "pandoc -N -f org -t pdf -o " (concat (file-name-as-directory send-to-mobile-directory) (concat basename ".pdf")) " " filename))
    (message "Invalid file, must be org file"))))

(defun open-org-inbox-file ()
  "Open org inbox file."
  (interactive)
  (find-file-other-window "~/notes/org/inbox.org"))

(defun open-org-agenda-file ()
  "Open org agenda file."
  (interactive)
  (find-file-other-window "~/notes/org/agenda.org"))

(map! :prefix "C-c"
      "i"   'open-org-inbox-file
      "a"   'open-org-agenda-file
      "m s" 'send-to-mobile
      "m p" 'org-mobile-push
      "m l" 'org-mobile-pull)
;;(keymap-global-set "C-c i" 'open-org-inbox-file)
;;(keymap-global-set "C-c a" 'open-org-agenda-file)
;;(keymap-global-set "C-c m s" 'send-to-mobile)
;;(keymap-global-set "C-c m p" 'org-mobile-push)
;;(keymap-global-set "C-c m l" 'org-mobile-pull)


(defun org-settings ()
  (visual-line-fill-column-mode)
  (setq visual-fill-column-width 90)
  (setq visual-fill-column-center-text t)
  (setq org-startup-indented t)
  (display-line-numbers-mode -1))
(add-hook 'org-mode-hook 'org-settings)

(setq org-directory "~/notes/org")
(setq org-mobile-directory "~/Koofr/mobileorg")
(setq org-mobile-inbox-for-pull "~/notes/org/inbox.org")
(setq org-mobile-files '("~/notes/org/mobileorg"))
(setq org-roam-directory (file-truename "~/notes/roam/"))
(setq org-roam-dailies-directory "journal/")

(use-package! org-roam
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r d" . org-id-get-create)
         ("C-c r c" . org-roam-capture)
         ("C-c r j" . org-roam-dailies-goto-today)
         ("C-c r g" . org-roam-dailies-goto-date))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:40}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      '(("m" "main" plain "%?"
         :if-new (file+head "main/${slug}.org"
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

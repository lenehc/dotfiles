;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;;
;;; UI

;; different font sizes for different machines
(if (equal (system-name) "CHECHE")
    (progn (setq doom-font                 (font-spec :family "MesloLGS Nerd Font" :size 14 :weight 'regular)
                 doom-variable-pitch-font  (font-spec :family "Georgia" :size 14 :weight 'medium))
           (after! org
             (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.0))))
  (setq doom-font                 (font-spec :family "MesloLGS Nerd Font" :size 12 :weight 'regular)
        doom-variable-pitch-font  (font-spec :family "Georgia" :size 14 :weight 'medium)))

(setq doom-theme 'doom-gruvbox
      display-line-numbers-type t
      confirm-kill-emacs nil)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

;; open zotero links in zotero
(require 'ol)
(org-link-set-parameters "zotero" :follow
                         (lambda (zpath)
                           (browse-url
                             (format "zotero:%s" zpath))))



;;
;;; Custom functions

(defun my/send-to-mobile ()
  "Export current buffer as PDF using Pandoc and send to mobile directory."
  (interactive)
  (let* ((filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (message "Processing current file...")
    (if (and (string-match-p
                (regexp-quote ".org") basename)
               (not (string-match-p
                     (regexp-quote "[") basename)))
    (shell-command
     (concat "pandoc -N -f org -t pdf -o " (concat (file-name-as-directory (expand-file-name my/send-to-mobile-directory)) (concat basename ".pdf")) " " filename))
    (message "Invalid file, must be org file."))))

(defun my/open-org-inbox ()
  "Open inbox.org in another window."
  (interactive)
  (find-file-other-window "~/org/inbox.org"))

(defun my/open-org-personal ()
  "Open personal.org in another window."
  (interactive)
  (find-file-other-window "~/org/personal.org"))

(defun my/open-org-school ()
  "Open school.org in another window."
  (interactive)
  (find-file-other-window "~/org/school.org"))

(defun my/org-roam-exclude-dailies-p (node)
  "Return non-nil if the NODE is not in ‘org-roam-dailies-directory’."
  (let ((file (org-roam-node-file node)))
    (not (string-prefix-p (expand-file-name org-roam-dailies-directory org-roam-directory) file))))

(defun my/org-roam-node-find-exclude-dailies ()
  "Find an Org-roam node, excluding nodes in the ‘org-roam-dailies-directory’."
  (interactive)
  (org-roam-node-find
   nil
   nil
   #'my/org-roam-exclude-dailies-p))

(defun my/org--make-preview-overlay (_ beg end image &optional imagetype)
  "Build an overlay between BEG and END using IMAGE file.
Argument IMAGETYPE is the extension of the displayed image,
as a string.  It defaults to \"png\"."
  (let ((ov (make-overlay beg end))
        (imagetype (or (intern imagetype) 'png)))
    (overlay-put ov 'org-overlay-type 'org-latex-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov
                 'modification-hooks
                 (list (lambda (o _flag _beg _end &optional _l)
                         (delete-overlay o))))
    (overlay-put ov
                 'display
                 (list 'image :type imagetype :file image :ascent 'center :margin '(0 . 5)))))
(advice-add 'org--make-preview-overlay :around #'my/org--make-preview-overlay)


;;
;;; Keybinds

(map! :leader
      "i" #'my/open-org-inbox
      "a" #'my/open-org-personal
      "s" #'my/open-org-school
      "c a" #'org-archive-subtree-default
      (:prefix "m"
        "s" #'my/send-to-mobile
        "p" #'org-mobile-push
        "l" #'org-mobile-pull)
      (:prefix "n"
        "l" #'org-roam-buffer-toggle
        "f" #'my/org-roam-node-find-exclude-dailies
        "i" #'org-roam-node-insert
        "d" #'org-id-get-create
        "c" #'org-roam-capture
        "j" #'org-roam-dailies-goto-today
        "g" #'org-roam-dailies-goto-date))


;;
;;; Org

(defun my/org-settings ()
  "Custom settings for org files."
  (visual-line-fill-column-mode)
  (org-fragtog-mode)
  (display-line-numbers-mode -1)
  (setq visual-fill-column-width 90)
  (setq visual-fill-column-center-text t))
(add-hook 'org-mode-hook #'my/org-settings)

(require 'org-roam)
(setq org-directory "~/org"
      org-mobile-directory "~/Koofr/mobileorg"
      org-mobile-inbox-for-pull "~/org/inbox.org"
      org-mobile-files '("~/org/mobileorg")
      org-roam-directory (file-truename "~/notes/roam/")
      org-roam-dailies-directory "journal/"
      my/send-to-mobile-directory "~/org"
      global-auto-revert-mode t)

(after! org
  (setq org-startup-indented t
        org-startup-with-latex-preview t))

(after! org-roam
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:40}" 'face 'org-tag))
        org-roam-capture-templates
        '(("m" "main" plain "%?"
           :if-new (file+head "main/${slug}.md" "---\ntitle: ${title}\n---\n")
           :unnarrowed t
           :empty-lines 1)
          ("w" "webdev" plain "%?"
           :if-new (file+head "webdev/${slug}.md" "---\ntitle: ${title}\n---\n")
           :unnarrowed t
           :empty-lines 1)
          ("s" "design" plain "%?"
           :if-new (file+head "design/${slug}.org" "#+title: ${title}\n\n")
           :unnarrowed t
           :empty-lines 1)
          ("d" "default" plain "%?"
           :if-new (file+head "%<%Y-%m-%d %H.%M.%S>-${slug}.md" "---\ntitle: ${title}\n---\n")
           :unnarrowed t
           :empty-lines 1))
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y>/%<%m>/%<%Y-%m-%d>.md" "---\ntitle: %<%Y-%m-%d>\n---\n")
           :unnarrowed t
           :empty-lines 1)))
  (org-roam-db-autosync-mode 1))

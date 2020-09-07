;;; -*- lexical-binding: t -*-
(setq-default lexical-binding t)

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1)))

;; Raising gc-cons threshold when the minibuffer is active

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216 ; 16mb
			  gc-cons-percentage 0.1) )))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(use-package git) ;; ensure we can install from git sources

(setq use-package-compute-statistics t)

(straight-use-package 'use-package-ensure-system-package)

;; Base function to create the home directory
(defun sr/fun/homedir (foldername)
  "Function to extract the home directory path"
  (expand-file-name foldername (getenv "HOME")))

;; Emacs directory defauling to .emacs.d
(defun sr/fun/emacs-dir (foldername)
  "Function to prepend the project directory path to any folder. Starts from the home directory."
  (expand-file-name foldername (sr/fun/homedir ".emacs.d" )))

(fset 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks t)
;; The default value seems to be 'ask

(setq load-prefer-newer t)

(use-package savehist
  :config
  (setq savehist-file "~/.emacs.d/savehist")
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  :hook (after-init-hook . savehist-mode))

(use-package saveplace
  :config
  (setq save-place-file "~/.emacs.d/saveplace")
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

(setq auto-save-default t)
(setq auto-save-timeout 20
      auto-save-interval 240)

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/"))

(unless (file-exists-p emacs-autosave-directory)
  (make-directory emacs-autosave-directory))

(setq auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(setq backup-directory-alist `((".*" . ,emacs-autosave-directory)))

(use-package backup-each-save
  :straight t
  :config (add-hook 'after-save-hook 'backup-each-save))

(setq vc-make-backup-files t)

(setq backup-by-copying t)

(setq kept-new-versions 10
      kept-old-verisons 0
      delete-old-versions t)

(straight-use-package 'smex)

(use-package ido
:config
(setq ido-enable-flex-matching t)
(ido-mode))

(load (sr/fun/emacs-dir "dotemacs.el"))

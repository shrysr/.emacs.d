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

 (let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
 (bootstrap-version 3))
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

;; Base function to create the home directory
(defun sr/fun/homedir (foldername)
"Function to extract the home directory path"
  (expand-file-name foldername (getenv "HOME")))

;; Emacs directory defauling to .emacs.d
(defun sr/fun/emacs-dir (foldername)
"Function to prepend the project directory path to any folder. Starts from the home directory."
  (expand-file-name foldername (sr/fun/homedir ".emacs.d" )))

(fset 'yes-or-no-p 'y-or-n-p)

(load (sr/fun/emacs-dir "dotemacs.el"))

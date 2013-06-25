; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-

;; Erlang mode for emacs
(setq load-path (cons ${emacsfile} load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; make sure all the right files open in erlang mode
(add-to-list 'auto-mode-alist '("\\.escript?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yrl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.xrl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.config?$" . erlang-mode))

;; Enable modes
(ido-mode 1)
(cua-mode 1)
(line-number-mode 1)
(column-number-mode 1)

(setq kill-whole-line t)
;; Disable some ui
;;(menu-bar-mode 0)
;;(scroll-bar-mode 0)
;;(tool-bar-mode 0)

;; Set the font
(set-face-attribute 'default nil :height 90 :weight 'bold)
;(set-face-background 'mode-line "black")
;(set-face-foreground 'mode-line "white")

;; Stop making ~ files, get rid of startup message
(setq inhibit-startup-message 0)
;(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)

;; Use clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Use spaces to indent, 4 by default.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-width 2)

;; delete trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; enable you to run rebar from within Emacs
(defun erlang-wtd (wtd-type)
  (format "in erlang-wtd")
  (cd "~/erlang-wtd/")
  (compile (concat "rebar " wtd-type))
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun wtd-compile ()
  (interactive)
  (format "in compile")
  (erlang-wtd "compile"))

(global-set-key [f12] 'wtd-compile)

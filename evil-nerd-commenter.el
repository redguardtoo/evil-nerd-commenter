;;; evil-nerd-commenter --- Comment/uncomment lines efficiently. Like Nerd Commenter in Vim

;; Copyright (C) 2012 Chen Bin
;; Author: Chen Bin <chenbin DOT sh AT gmail>
;; URL: http://github.com/redguardtoo/evil-nerd-commenter
;; Keywords: commenter vim line evil
;; Version: 0.0.4

;; This file is not part of GNU Emacs.

;; This file is free software (GPLv3 License)

;; How to set it up:
;; 1. "evilnc-default-hotkeys" will assign hotkey "M-;" for emacs normal mode and "<Leader>ci" for evil-mode.
;; 2. Calling (evilnc-default-hotkeys) is NOT needed if you define your own hotkey.
;;
;; How to use:
;; 1. "evilnc-comment-or-uncomment-lines" - comment/uncomment lines.
;; 2. "evilnc-comment-or-uncomment-to-the-line" - comment/uncomment from current line to the specified line

;;; Code:

;; shamelessly copied from goto-line
(defun evilnc--goto-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line))))
  )

(defun evilnc--fix-buggy-major-modes ()
  "fix major modes whose comment regex is buggy.
@see http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-03/msg00891.html"
  (when (string= major-mode "autoconf-mode")
    ;; since comment-use-syntax is nil in autoconf.el, the comment-start-skip need
    ;; make sure the its first parenthesized expression match the string exactly before
    ;; the "dnl", check the comment-start-skip in lisp-mode may give you some hint.
    ;; See code in (defun comment-search-forward) from emacs 24.2.1:
    ;; (if (not comment-use-syntax)
    ;;     (if (re-search-forward comment-start-skip limit noerror)
    ;;     (or (match-end 1) (match-beginning 0))
    ;; My regex make sure (match-end 1) return the position of comment starter
    (when (and (boundp 'comment-use-syntax) (not comment-use-syntax))
        ;; Maybe autoconf.el will (setq comment-use-syntax t) in the future?
        (setq comment-start-skip "^\\(\\s*\\)\\(dnl\\|#\\) +")
      )
    )
  )

;;;###autoload
(defun evilnc-comment-or-uncomment-to-the-line (&optional LINENUM)
  "Comment or uncomment from the current line to the LINENUM line"
  (interactive "p")
  (if (not (region-active-p))
      (let ((b (line-beginning-position))
            (e (line-end-position)))
        (save-excursion
          (evilnc--goto-line LINENUM)
          (if (< (line-beginning-position) b)
              (setq b (line-beginning-position)))
          (if (> (line-end-position) e)
              (setq e (line-end-position)))
          (evilnc--fix-buggy-major-modes)
          (comment-or-uncomment-region b e)
          )
        )
    ))

;;;###autoload
(defun evilnc-toggle-comment-empty-lines ()
  (interactive)
  (if comment-empty-lines
      (setq comment-empty-lines nil)
    (setq comment-empty-lines t)
    )
  (message (if comment-empty-lines
               "Empty line(s) will be commented"
             "Empty line(s) will NOT be commented"))
  )

;;;###autoload
(defun evilnc-comment-or-uncomment-lines (&optional NUM)
  "Comment or uncomment lines.
   Case 1: If no region selected, comment/uncomment on current line. if NUM>1, comment/uncomment
   extra N-1 lines from next line
   Case 2: If a region selected, the region is expand to make sure the region contain
   whole lines. Then we comment/uncomment the expanded region. NUM is ignored."
  (interactive "p")
  (if (not (region-active-p))
      (let ((b (line-beginning-position))
            e)
        (save-excursion
          (forward-line (- NUM 1))
          (setq e (line-end-position))
          (evilnc--fix-buggy-major-modes)
          (comment-or-uncomment-region b e)
          )
        )
    ;; expand selected region
    (save-excursion
      (let ((b (region-beginning))
            (e (region-end))
            )
        (goto-char b)
        (setq b (line-beginning-position))
        (goto-char e)
        (setq e (line-end-position))
        (evilnc--fix-buggy-major-modes)
        (comment-or-uncomment-region b e)))
    ))

;;;###autoload
(defun evilnc-default-hotkeys ()
  "Set the hotkeys of evil-nerd-comment"
  (interactive)
  (global-set-key "\M-;" 'evilnc-comment-or-uncomment-lines)
  (global-set-key "\M-:" 'evilnc-comment-or-uncomment-to-the-line)
  )

(provide 'evil-nerd-commenter)

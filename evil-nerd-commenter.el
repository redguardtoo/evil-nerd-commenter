;;; evil-nerd-commenter --- Comment/uncomment lines efficiently. Like Nerd Commenter in Vim

;; Copyright (C) 2012 Chen Bin
;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/evil-nerd-commenter
;; Keywords: commenter vim line evil
;; Version: 0.0.2

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
(defun evilnc-goto-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (if (eq selective-display t)
	(re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line))))
  )

;;;###autoload
(defun evilnc-comment-or-uncomment-to-the-line (&optional LINENUM)
  "Comment or uncomment from the current line to the LINENUM line"
  (interactive "p")
  (if (not (region-active-p))
      (let ((b (line-beginning-position))
            (e (line-end-position)))
        (save-excursion
          (evilnc-goto-line LINENUM)
          (if (< (line-beginning-position) b)
              (setq b (line-beginning-position)))
          (if (> (line-end-position) e)
              (setq e (line-end-position)))
          (comment-or-uncomment-region b e)
          )
        )
  ))

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

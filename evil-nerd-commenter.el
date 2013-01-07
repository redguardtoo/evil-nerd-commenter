;;; evil-nerd-commenter --- Comment/uncomment lines efficiently. Like Nerd Commenter in Vim

;; Copyright (C) 2012 Chen Bin
;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/evil-nerd-commenter
;; Keywords: commenter vim line evil
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This file is free software (GPLv3 License)

;; How to set it up:
;; 1. There is ONLY one command "evilnc-comment-or-uncomment-lines".
;; 2. "evilnc-default-hotkeys" will assign hotkey "M-;" for emacs normal mode and "<Leader>ci" for evil-mode.
;; 3. Calling (evilnc-default-hotkeys) is NOT needed if you define your own hotkey.

;;; Code:

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
  ;; set hot key for evil-mode, package evil-leader is needed
  (if (fboundp 'evil-leader/set-key)
      ;; same as nerd commenter in vim
      (evil-leader/set-key "ci" 'evilnc-comment-or-uncomment-lines))
  )

(provide 'evil-nerd-commenter)

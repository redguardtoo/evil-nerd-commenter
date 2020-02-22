;;; evil-nerd-commenter-sdk.el --- SDK used by other files

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; SDK used internally

;;; Code:

(defun evilnc--check-fonts (fonts-under-cursor fonts-list)
  "Check whether FONTS-UNDER-CURSOR among FONTS-LIST."
  (delq nil
        (mapcar #'(lambda (f)
                    ;; learn this trick from flyspell
                    (member f fonts-list))
                fonts-under-cursor)))

(defun evilnc-web-mode-is-comment (&optional pos)
  "Check whether the code at POS is comment.
`web-mode' removes its API, so create our own."
  (unless pos (setq pos (point)))
  (not (null (or (eq (get-text-property pos 'tag-type) 'comment)
                 (eq (get-text-property pos 'block-token) 'comment)
                 (eq (get-text-property pos 'part-token) 'comment)))))

(defun evilnc-fonts-at-point (pos)
  "Get font faces at POS."
  (let* ((fontfaces (if (> pos 0) (get-text-property pos 'face))))
    (if (listp fontfaces) fontfaces (list fontfaces))))

(defun evilnc-is-pure-comment (pos)
  "Check character at POS is pure comment."
  (or (and (eq major-mode 'web-mode)
           (evilnc-web-mode-is-comment pos))
      (evilnc--check-fonts (evilnc-fonts-at-point pos)
                           '(font-lock-comment-face
                             font-lock-comment-delimiter-face))))

(defun evilnc-is-whitespace (pos)
  "Character at POS is white space."
  (member (evilnc-get-char pos) '(32 9)))

(defun evilnc-is-line-end (pos)
  "Character at POS is line end."
  (member (evilnc-get-char pos) '(10 11)))

(defun evilnc-is-comment (pos)
  "Check whether the code at POS is comment by comparing font face.
Please note the white spaces out of comment is treated as comment,
or else we can't select multiple lines comment."
  (cond
   ((or (< pos (point-min)) (> pos (point-max)))
    nil)
   ((not (evilnc-fonts-at-point pos))
    ;; character under cursor is SPACE or TAB
    ;; and out of comment
    (evilnc-is-whitespace pos))
   (t
    (evilnc-is-pure-comment pos))))

(defun evilnc-get-char (pos)
  "Get character at POS."
  (save-excursion
    (goto-char pos)
    (following-char)))

(defun evilnc-is-comment-delimiter (pos)
  "Is character at POS a comment delimiter?"
  (let* ((fontfaces (evilnc-fonts-at-point pos)))
    (and fontfaces
         (evilnc--check-fonts fontfaces
                              '(font-lock-comment-delimiter-face)))))

(defun evilnc-sdk-inside-one-line-p (beg end)
  "Test BEG and END is inside one line."
  (and (<= (line-beginning-position) beg)
       (<= end (line-end-position))))

(defun evilnc-sdk-expand-to-contain-whole-lines (beg end)
  "Expand region between BEG and END so the region contain whole lines.
Return new range like '(region_begin . region_end)."
  (save-excursion
    ;; Another work around for evil-visual-line bug:
    ;; In `evil-mode', if we use hotkey V or `evil-visual-line' to select line,
    ;; the (line-beginning-position) of the line which is after the last selected
    ;; line is always (region-end)! Don't know why.
    (when (and (> end beg)
               (save-excursion (goto-char end) (= end (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
      (setq end (1- end)))

    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position)))
  (cons beg end))

(provide 'evil-nerd-commenter-sdk)
;;; evil-nerd-commenter-sdk.el ends here

;;; evil-nerd-commenter-operator.el --- Provides an evil operator for evil-nerd-commenter

;; Copyright (C) 2013-2015, Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/evil-nerd-commenter
;; Version: 2.3.3
;; Keywords: commenter vim line evil
;;
;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-nerd-commenter
;;
;; evil-nerd-commenter is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-nerd-commenter is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; Provides operators for evil-mode.

;;; Code:

(require 'evil nil 'noerror)

(evil-define-operator evilnc-comment-operator (beg end type)
  "Comments text from BEG to END with TYPE."
  (interactive "<R>")
  (cond
   ((eq type 'block)
    (let* ((newpos (evilnc--extend-to-whole-comment beg end) ))
      (evil-apply-on-block #'evilnc--comment-or-uncomment-region
                           (nth 0 newpos)
                           (nth 1 newpos)
                           nil)))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (evilnc--comment-or-uncomment-region (1- beg) end))

   ((eq type 'line)
    (evilnc--comment-or-uncomment-region beg (1- end)))

   (t
    (let* ((newpos (evilnc--extend-to-whole-comment beg end) ))
      (evilnc--comment-or-uncomment-region (nth 0 newpos) (nth 1 newpos)))))

  ;; place cursor on beginning of line
  (if (and (called-interactively-p 'any)
           (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator evilnc-copy-and-comment-operator (beg end)
  "Inserts an out commented copy of the text from BEG to END."
  :move-point (not evilnc-original-above-comment-when-copy-and-comment)
  (interactive "<r>")
    (evil-yank-lines beg end nil 'lines)
    (cond
     (evilnc-original-above-comment-when-copy-and-comment
      (let* ((p (point)))
        (comment-region beg end)
        (goto-char beg)
        (evil-paste-before 1)
        (goto-char p)))
     (t
      (goto-char end)
      (evil-paste-before 1)
      ;; actual comment operatio should happen at last
      ;; or else beg end will be screwed up
      (comment-region beg end))))

(provide 'evil-nerd-commenter-operator)
;;; evil-nerd-commenter-operator.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

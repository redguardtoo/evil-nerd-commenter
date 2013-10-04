;;; evil-nerd-commenter --- Comment/uncomment lines efficiently. Like Nerd Commenter in Vim

;; Copyright (C) 2013 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/evil-nerd-commenter
;; Version: 1.2.4
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
;; This program emulates nerd-commenter.vim by Marty Grenfell.
;; It help you comment/uncomment multiple lines without selecting them.
;;
;; `M-x evilnc-default-hotkeys` assigns hotkey `M-;` to `evilnc-comment-or-uncomment-lines`
;; `M-x evilnc-comment-or-uncomment-lines` comment or uncomment lines.
;; `M-x evilnc-comment-or-uncomment-to-the-line` will comment/uncomment from current line to
;; the specified line number. The line number is passed as parameter of the command.
;; For example, `C-u 99 evilnc-comment-or-uncomment-to-the-line` will comment code from
;; current line to line 99.
;;
;; Though this program could be used *independently*, I highly recommend you use it with
;; evil (http://gitorious.org/evil)
;;
;; Evil makes you take advantage of power of Vi to comment lines in shocking speed.
;; For example, you can press key `99,ci` to comment out 99 lines.


;;; Code:

;; Example, presss ",,a{" will change C code:
;;   {printf("hello");} => /* {printf("hello");}*/
;; google "vim text object for more syntax"
(defcustom evilnc-hotkey-comment-operator ",," "The hot key for evilnc-comment-operator to (un)comment text object"
  :type 'string
  :group 'evil-nerd-commenter)

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

(defun evilnc--operation-on-lines-or-region (fn &optional NUM)
  (if (not (region-active-p))
      (let ((b (line-beginning-position))
            e)
        (save-excursion
          (forward-line (- NUM 1))
          (setq e (line-end-position))
          )
        (funcall fn b e)
        )
    ;; expand selected region
    (progn
      (save-excursion
        (let ((b (region-beginning))
              (e (region-end))
              )
          ;; another work around for evil-visual-line bug:
          ;; in evil-mode, if we use hot key V `M-x evil-visual-line` to select line
          ;; the (line-beginning-position) of the line which is after the last selected
          ;; line is always (region-end)! Don't know why.
          (if (and (> e b) (= e (line-beginning-position)) (boundp 'evil-state) (string= evil-state 'visual))
              (setq e (1- e))
            )
          (goto-char b)
          (setq b (line-beginning-position))
          (goto-char e)
          (setq e (line-end-position))
          (funcall fn b e)
          ))
      )
    )
  )


(defun evilnc--get-one-paragraph-region ()
  (let (b e)
    (save-excursion
      (setq b (re-search-backward "^[ \t]*$" nil t))
      (if b (progn
              (forward-line)
              (setq b (line-beginning-position))
              )
        (setq b 1)
        )
      )
    (save-excursion

      (setq e (re-search-forward "^[ \t]*$" nil t))
      (if e (progn
              (forward-line -1)
              (setq e (line-end-position))
              )
        (setq e (point-max))
        )
      )
    (list b e)
    )
  )

(defun evilnc--in-comment-p (pos)
  (interactive)
  (let ((fontface (get-text-property pos 'face)))
    ;; learn this trick from flyspell
    (or (string= fontface 'font-lock-comment-face)
        (string= fontface 'font-lock-comment-delimiter-face)
        )
    ))

;; @return (list beg end)
(defun evilnc--extend-to-whole-comment (beg end)
  (interactive)
  (if (evilnc--in-comment-p beg)
      (save-excursion
        (let ((newbeg beg)
              (newend end))

          ;; extend the beginning
          (goto-char newbeg)
          (while (and (>= newbeg (line-beginning-position)) (evilnc--in-comment-p newbeg))
            (decf newbeg)
            )
          ;; make sure newbeg is at the beginning of the comment
          (if (< newbeg beg) (incf newbeg))

          ;; extend the end
          (goto-char newend)
          (while (and (<= newend (line-end-position)) (evilnc--in-comment-p newend))
            (incf newend)
            )
          ;; make sure newend is at the end of the comment
          (if (> newend end) (decf newend))

          (list newbeg newend)
          )
        )
    (list beg end)
    ))


;; ==== below this line are public commands
;;;###autoload
(defun evilnc-comment-or-uncomment-paragraphs (&optional NUM)
  "Comment or uncomment paragraph(s). A paragraph is a continuation non-empty lines.
Paragraphs are separated by empty lines."
  (interactive "p")
  (let ((i 0)
        rlt
        (b (point-max))
        (e 0)
        )
    (catch 'break
      (while (< i NUM)
        (incf i)
        (setq rlt (evilnc--get-one-paragraph-region))
        (setq b (if (< (nth 0 rlt) b) (nth 0 rlt) b))
        (setq e (if (> (nth 1 rlt) e) (nth 1 rlt) e))

        ;; prepare for the next paragraph
        (if (and rlt (< i NUM))
            (progn
              ;; e should be the end of last non-empty line
              (goto-char e)

              ;; move to an empty line
              (forward-line)

              ;; move to next non-empty line
              (re-search-forward "^[ \t]*[^ \t]" nil t)

              (if (<= (line-beginning-position) e)
                  (progn
                    (throw 'break i)
                    )
                )
              )
          (progn
            (throw 'break i)
            )
          )
        ))
    (when (<= b e)
      (save-excursion
        (evilnc--fix-buggy-major-modes)
        (comment-or-uncomment-region b e)
        )
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
  (evilnc--operation-on-lines-or-region '(lambda (b e)
                                           (evilnc--fix-buggy-major-modes)
                                           (comment-or-uncomment-region b e)
                                           )
                                        NUM)
  )

;;;###autoload
(defun evilnc-copy-and-comment-lines (&optional NUM)
  "Copy and paste lines. Then comment original lines.
   Case 1: If no region selected, operate on current line. if NUM>1, comment/uncomment
   extra N-1 lines from next line
   Case 2: If a region selected, the region is expand to make sure the region contain
   whole lines. Then we operate the expanded region. NUM is ignored.
"
  (interactive "p")
  (evilnc--operation-on-lines-or-region
   '(lambda (beg end)
      (evilnc--fix-buggy-major-modes)
      (let ((str (buffer-substring-no-properties beg end)))
        (goto-char end)
        (newline 1)
        (insert-before-markers str)
        (comment-region beg end)
        ))
   NUM)
  )

;;;###autoload
(defun evilnc-default-hotkeys ()
  "Set the hotkeys of evil-nerd-comment"
  (interactive)
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-to-the-line)
  (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
  (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
  (eval-after-load 'evil
    '(progn
       (define-key evil-normal-state-map ",ci" 'evilnc-comment-or-uncomment-lines)
       (define-key evil-normal-state-map ",cl" 'evilnc-comment-or-uncomment-to-the-line)
       (define-key evil-normal-state-map ",cc" 'evilnc-copy-and-comment-lines)
       (define-key evil-normal-state-map ",cp" 'evilnc-comment-or-uncomment-paragraphs)
       (define-key evil-normal-state-map ",cr" 'comment-or-uncomment-region)
       )))

;; Attempt to define the operator on first load.
;; Will only work if evil has been loaded
(eval-after-load 'evil
  '(progn
     (evil-define-operator evilnc-comment-operator (beg end type register yank-handler)
       "Comments text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
       (interactive "<R><x><y>")
       (unless register
         (let ((text (filter-buffer-substring beg end)))
           (unless (string-match-p "\n" text)
             ;; set the small delete register
             (evil-set-register ?- text))))
       (evil-yank beg end type register yank-handler)
       (cond
        ((eq type 'block)
         (let ((newpos (evilnc--extend-to-whole-comment beg end) ))
           (evil-apply-on-block #'comment-or-uncomment-region (nth 0 newpos) (nth 1 newpos) nil)
           )
         )
        ((and (eq type 'line)
              (= end (point-max))
              (or (= beg end)
                  (/= (char-before end) ?\n))
              (/= beg (point-min))
              (=  (char-before beg) ?\n))
         (comment-or-uncomment-region (1- beg) end))
        (t
         (let ((newpos (evilnc--extend-to-whole-comment beg end) ))
           (comment-or-uncomment-region (nth 0 newpos) (nth 1 newpos))
           )
         ))
       ;; place cursor on beginning of line
       (when (and (evil-called-interactively-p)
                  (eq type 'line))
         (evil-first-non-blank)))
     (define-key evil-normal-state-map evilnc-hotkey-comment-operator 'evilnc-comment-operator)
     (define-key evil-visual-state-map evilnc-hotkey-comment-operator 'evilnc-comment-operator)
     ))

(provide 'evil-nerd-commenter)

;;; evil-nerd-commenter.el ends here

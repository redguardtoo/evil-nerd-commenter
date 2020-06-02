;; evil-nerd-commenter-tests.el --- unit tests for evil-nerd-commenter -*- coding: utf-8 -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

(require 'ert)
(require 'evil-nerd-commenter)
(require 'js)

(defun evilnc-get-lines (start end)
  (split-string (buffer-substring-no-properties start end) "\n"))

(ert-deftest evilnc-test-forward-line ()
  (with-temp-buffer
    (insert "hello\nworld\nbye\nworld")
    (goto-char (point-min))
    (evilnc--forward-line 1)
    (should (eq (length (evilnc-get-lines (point-min) (point))) 2))
    (goto-char (point-min))
    (evilnc--forward-line 2)
    (should (eq (length (evilnc-get-lines (point-min) (point))) 3))))

(ert-deftest evilnc-test-comment-lines ()
  (let* (lines)
    (with-temp-buffer
      (insert "hello\nworld\nbye\nworld")
      ;; test js comment
      (js-mode)
      (goto-char (point-min))
      ;; comment out current line
      (evilnc-comment-or-uncomment-lines 1)
      (setq lines (evilnc-get-lines (point-min) (line-end-position)))
      (should (string= (car lines) "// hello"))

      ;; un-comment current line
      (evilnc-comment-or-uncomment-lines 1)
      (setq lines (evilnc-get-lines (point-min) (line-end-position)))
      (should (string= (car lines) "hello"))

      ;; comment multiple lines
      (goto-char (point-min))
      (evilnc-comment-or-uncomment-lines 3)
      (setq lines (evilnc-get-lines (point-min) (point-max)))
      (should (string= (nth 0 lines) "// hello"))
      (should (string= (nth 1 lines) "// world"))
      (should (string= (nth 2 lines) "// bye"))
      (should (string= (nth 3 lines) "world")))))

(ert-deftest evilnc-test-copy-and-comment-lines ()
  (let* (lines)
    (with-temp-buffer
      (insert "hello\nworld")
      (js-mode)
      (goto-char (point-min))
      (evilnc-copy-and-comment-lines 2)
      (should (not (evilnc-pure-comment-p (point))))
      (setq lines (evilnc-get-lines (point-min) (point-max)))
      (should (string= (nth 0 lines) "// hello"))
      (should (string= (nth 1 lines) "// world"))
      (should (string= (nth 2 lines) "hello"))
      (should (string= (nth 3 lines) "world")))))

(defun evilnc-uncomment-to-original-html ()
  ;; move the cursor to the middle of html tag and uncomment
  (goto-char (point-min))
  (forward-line 1)
  ;; uncomment whole tag
  (evilnc-comment-or-uncomment-html-tag)
  ;; should be same as original html
  (let* ((lines (evilnc-get-lines (point-min) (point-max))))
    (should (string= (nth 0 lines) "<div class=\"box\">"))
    (should (string= (nth 1 lines) "hello world"))
    (should (string= (nth 2 lines) "</div>"))))

(ert-deftest evilnc-test-comment-html-tag ()
  (let* (lines)
    (with-temp-buffer
      (insert "<div class=\"box\">\nhello world\n</div>")


      ;; comment tag in html file
      (html-mode)
      (goto-char (point-min))
      (evilnc-comment-or-uncomment-html-tag)
      (setq lines (evilnc-get-lines (point-min) (point-max)))
      (should (string= (nth 0 lines) "<!-- <div class=\"box\">"))
      (should (string= (nth 1 lines) "hello world"))
      (should (string= (nth 2 lines) "</div> -->"))

      (evilnc-uncomment-to-original-html)

      ;; comment tag in html file
      (js-mode)
      (goto-char (point-min))
      (evilnc-comment-or-uncomment-html-tag)
      (setq lines (evilnc-get-lines (point-min) (point-max)))
      (should (string= (nth 0 lines) "{/* <div class=\"box\">"))
      (should (string= (nth 1 lines) "hello world"))
      (should (string= (nth 2 lines) "</div> */}"))

      (evilnc-uncomment-to-original-html))))

(ert-deftest evilnc-test-org-src-block ()
  (let* (lang-f)
    (with-temp-buffer
      (insert "* hello\n"
              "** world\n"
              "#+BEGIN_SRC python\n"
              "def f():\n"
              "    print 'hello world'\n"
              "    print 'bye world'\n"
              "#+END_SRC\n")
      (org-mode)
      (goto-char (point-min))
      (re-search-forward "print 'hello world'")
      (setq lang-f (evilnc--org-lang-major-mode (evilnc--org-src-block-info)))
      (should (string= lang-f "python-mode")))))

(ert-deftest evilnc-test-org-elisp-src-block()
  (let* (lang-f)
    (with-temp-buffer
      (insert "* hello\n"
              "** world\n"
              "#+BEGIN_SRC elisp\n"
              "(defun f ()\n"
              "  (princ 'hello world')\n"
              "  (princ 'bye world'))\n"
              "#+END_SRC\n")
      (org-mode)
      (goto-char (point-min))
      (re-search-forward "(princ 'hello world'")
      (setq lang-f (evilnc--org-lang-major-mode (evilnc--org-src-block-info)))
      (should (string= lang-f "emacs-lisp-mode")))))

(ert-deftest evilnc-test-paragraph-line-calculation ()
  (let* (selected-region)
    (with-temp-buffer
      (insert "hello world\n"
              "\n"
              "para1 begin\n"
              "line 4\n"
              "line 5\n"
              "para1 end\n"
              "\n"
              "bye world")

      ;; test paragraph selection
      (goto-char (point-min))
      ;; begin of paragraph
      (re-search-forward "para1 begin")
      (setq selected-region (evilnc--get-one-paragraph-region))
      (should (eq 14 (car selected-region)))
      (should (eq 49 (cadr selected-region)))
      ;; middle of paragraph
      (re-search-forward "line 4")
      (setq selected-region (evilnc--get-one-paragraph-region))
      (should (eq 14 (car selected-region)))
      (should (eq 49 (cadr selected-region)))

      ;; test line number
      (goto-char (point-min))
      ;; move focus to line 4
      (re-search-forward "line 4")
      ;; line 14 is close
      (should (eq 14 (evilnc--find-destination-linenum 4)))
      ;; line 5 is close to line 4
      (should (eq 5 (evilnc--find-destination-linenum 5)))
      ;; line 9 is close to line 4
      (should (eq 9 (evilnc--find-destination-linenum 9)))
      ;; move focus to line 5
      (re-search-forward "line 5")
      ;; line 14 is close, ignore the lines abover current line (line 5)
      (should (eq 14 (evilnc--find-destination-linenum 4)))
      ;; line 5 is close to line 4
      (should (eq 15 (evilnc--find-destination-linenum 5)))
      ;; line 9 is close to line 4
      (should (eq 9 (evilnc--find-destination-linenum 9))))))

(ert-deftest evilnc-test-org-src-block-info ()
  (let* (info)
    (with-temp-buffer
      (insert "* test\n"
              "test\n"
              "#+BEGIN_SRC python\n"
              "hello world\n"
              "line\n"
              "#+END_SRC\n\n"
              "* heading\n"
              "subtext\n")

      ;; move foucs to the middle of source block
      (goto-char (point-min))
      (search-forward "hello world")
      ;; extract src block info
      (setq info (evilnc--org-src-block-info))
      (should (eq 32 (nth 0 info)))
      (should (eq 49 (nth 1 info)))
      (should (string= "python" (nth 2 info))))))

(ert-deftest evilnc-test-expand-whole ()
  (let* (info)
    (with-temp-buffer
      (insert "console.log('hello world');\n"
              "console.log('hello world');\n"
              "console.log('hello world');")

      (goto-char (point-min))
      ;; move foucs to the middle the line
      (search-forward "hello world")
      (should (evilnc-sdk-inside-one-line-p (point) (1- (line-end-position))))
      (should (not (evilnc-sdk-inside-one-line-p (point) (1- (point-max)))))
      (let* ((b (point))
             e
             range)
        (search-forward "hello world")
        (search-forward "hello world") ; middle of third line
        (setq e (point))
        (should (not (evilnc-sdk-inside-one-line-p b e)))
        (setq range (evilnc-sdk-expand-to-contain-whole-lines b e))
        (should (eq (car range) (save-excursion (goto-char b) (line-beginning-position))))
        (should (eq (cdr range) (save-excursion (goto-char e) (line-end-position))))))))

(ert-run-tests-batch-and-exit)

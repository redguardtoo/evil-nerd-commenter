;; evil-nerd-commenter-tests.el --- unit tests for evil-nerd-commenter -*- coding: utf-8 -*-

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
      (should (not (evilnc-is-pure-comment (point))))
      (setq lines (evilnc-get-lines (point-min) (point-max)))
      (should (string= (nth 0 lines) "// hello"))
      (should (string= (nth 1 lines) "// world"))
      (should (string= (nth 2 lines) "hello"))
      (should (string= (nth 3 lines) "world")))))

(ert-deftest evilnc-test-comment-html-tag ()
  (let* (lines)
    (with-temp-buffer
      (insert "<div class=\"box\">\nhello world\n</div>")
      (html-mode)
      ;; comment tag
      (goto-char (point-min))
      (evilnc-comment-or-uncomment-html-tag)
      (setq lines (evilnc-get-lines (point-min) (point-max)))
      (should (string= (nth 0 lines) "{/* <div class=\"box\">"))
      (should (string= (nth 1 lines) "hello world"))
      (should (string= (nth 2 lines) "</div> */}"))
      ;; move the cursor to the middle of html tag
      (goto-line 2)
      ;; uncomment whole tag
      (evilnc-comment-or-uncomment-html-tag)
      (setq lines (evilnc-get-lines (point-min) (point-max)))
      (should (string= (nth 0 lines) "<div class=\"box\">"))
      (should (string= (nth 1 lines) "hello world"))
      (should (string= (nth 2 lines) "</div>")))))

(ert-deftest evilnc-test-org-src-block ()
  (let* (lang-f)
    (with-temp-buffer
      (insert "* hello\n"
              "** world\n"
              "#+BEGIN_SRC python\n"
              "def f():"
              "    print 'hello world'\n"
              "    print 'bye wrold'\n"
              "#+END_SRC\n")
      (org-mode)
      (goto-char (point-min))
      (re-search-forward "print 'hello world'")
      (setq lang-f (evilnc--org-lang-major-mode))
      (should (string= lang-f "python-mode")))))

(ert-run-tests-batch-and-exit)

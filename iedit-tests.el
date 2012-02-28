;;; iedit-tests.el --- iedit's automatic-tests

;; Copyright (C) 2010, 2011, 2012 Victor Ren

;; Time-stamp: <2012-02-28 10:23:08 Victor Ren>
;; Author: Victor Ren <victorhge@gmail.com>
;; Version: 0.94
;; X-URL: http://www.emacswiki.org/emacs/Iedit

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is part of iedit.

;;; Code:
(require 'ert)

(ert-deftest iedit-compile-test ()
  (let ((byte-compile-error-on-warn t ))
    (should (byte-compile-file "iedit.el"))
    (delete-file "iedit.elc" nil)))


(defun with-iedit-test-fixture (input-buffer-string body)
  "iedit test fixture"
  (unwind-protect
      (with-temp-buffer
        (insert input-buffer-string)
        (goto-char 1)
        (iedit-mode)
        (funcall body))))

(ert-deftest iedit-mode-base-test ()
  (with-iedit-test-fixture
"foo
  foo
   barfoo
   foo"
   (lambda ()
     (should (= 3 (length iedit-occurrences-overlays)))
     (should (string= iedit-initial-string-local "foo"))
     (set-mark-command nil)
     (forward-line 2)
     (iedit-mode)
     (should (= 2 (length iedit-occurrences-overlays)))
     (iedit-mode)
     (should (null iedit-occurrences-overlays)))))

(ert-deftest iedit-movement-test ()
  (with-iedit-test-fixture
"foo
  foo
   barfoo
   foo "
   (lambda ()
     (iedit-last-occurrence)
     (should (= (point) 24))
     (iedit-first-occurrence)
     (should (= (point) 1))
     (iedit-next-occurrence)
     (should (= (point) 7))
     (iedit-next-occurrence)
     (should (= (point) 24))
     (iedit-next-occurrence)
     (should (= (point) 24))
     (should (string= (current-message) "This is the last occurrence."))
     (iedit-next-occurrence)
     (should (= (point) 1))
     (should (string= (current-message) "Located the first occurrence."))
     (iedit-next-occurrence)
     (should (= (point) 7))
     (goto-char (point-max))
     (iedit-prev-occurrence)
     (should (= (point) 27))
     (iedit-prev-occurrence)
     (should (= (point) 24))
     (iedit-prev-occurrence)
     (should (= (point) 7))
     (iedit-prev-occurrence)
     (should (= (point) 1))
     (iedit-prev-occurrence)
     (should (= (point) 1))
     (should (string= (current-message) "This is the first occurrence."))
     (iedit-prev-occurrence)
     (should (= (point) 24))
     (should (string= (current-message) "Located the last occurrence."))
     )))

(ert-deftest iedit-mode-with-region-test ()
  (with-iedit-test-fixture
"foobar
 foo
 foo
 bar
foo"
   (lambda ()
     (iedit-mode)
     (goto-char 1)
     (set-mark-command nil)
     (forward-char 3)
     (iedit-mode)
     (should (= 4 (length iedit-occurrences-overlays)))
     (should (string= iedit-initial-string-local "foo"))
     (should (null iedit-only-complete-symbol-local))
     (goto-char 1)
     (set-mark-command nil)
     (forward-line 3)
     (iedit-mode 4)
     (should (= 1 (length iedit-occurrences-overlays))))))

(ert-deftest iedit-occurrence-update-test ()
  (with-iedit-test-fixture
"foo
  foo
   barfoo
   foo"
   (lambda ()
     (insert "1")
     (should (string= (buffer-string)
"1foo
  1foo
   barfoo
   1foo"))
     (backward-delete-char 1)
     (should (string= (buffer-string) input-buffer-string))
     (capitalize-word 1)
     (should (string= (buffer-string)
"Foo
  Foo
   barfoo
   Foo"))
     ;; test insert from empty
     (iedit-delete-occurrences)
     (insert "1")
     (should (string= (buffer-string)
"1
  1
   barfoo
   1")))))

(ert-deftest iedit-toggle-case-sensitive-test ()
  (with-iedit-test-fixture
"foo
  Foo
   barfoo
   foo"
   (lambda ()
     (should (= 2 (length iedit-occurrences-overlays)))
     (iedit-toggle-case-sensitive)
     (should (= 3 (length iedit-occurrences-overlays)))
     (iedit-next-occurrence)
     (iedit-toggle-case-sensitive)
     (should (= 1 (length iedit-occurrences-overlays))))))

(ert-deftest iedit-apply-on-occurrences-test ()
  "Test functions deal with the whole occurrences"
  (with-iedit-test-fixture
"foo
  foo
   barfoo
   foo"
   (lambda ()
     (iedit-upcase-occurrences)
     (should (string= (buffer-string)
"FOO
  FOO
   barfoo
   FOO"))
     (iedit-downcase-occurrences)
     (should (string= (buffer-string) input-buffer-string))
     (iedit-replace-occurrences "bar")
     (should (string= (buffer-string)
"bar
  bar
   barfoo
   bar"))
     (iedit-number-occurrences 1)
     (should (string= (buffer-string)
"1 bar
  2 bar
   barfoo
   3 bar")))))


(ert-deftest iedit-blank-occurrences-test ()
  "Test functions deal with the whole occurrences"
  (with-iedit-test-fixture
"foo foo barfoo foo"
   (lambda ()
     (iedit-blank-occurrences)
     (should (string= (buffer-string) "        barfoo    ")))))

(ert-deftest iedit-delete-occurrences-test ()
  "Test functions deal with the whole occurrences"
  (with-iedit-test-fixture
"foo foo barfoo foo"
   (lambda ()
     (iedit-delete-occurrences)
     (should (string= (buffer-string) "  barfoo ")))))

(ert-deftest iedit-toggle-buffering-test ()
  (with-iedit-test-fixture
"foo
 foo
  barfoo
    foo"
   (lambda ()
     (iedit-toggle-buffering)
     (insert "bar")
     (should (string= (buffer-string)
"barfoo
 foo
  barfoo
    foo"))
     (iedit-toggle-buffering)
     (should (string= (buffer-string)
"barfoo
 barfoo
  barfoo
    barfoo"))
     (should (= (point) 4))
     (iedit-toggle-buffering)
     (backward-delete-char 3)
     (should (string= (buffer-string)
"foo
 barfoo
  barfoo
    barfoo"))
     (goto-char 15) ;not in an occurrence
     (should (null (iedit-find-current-occurrence-overlay)))
     (iedit-toggle-buffering)
     (should (string= (buffer-string)
"foo
 barfoo
  barfoo
    barfoo")))))

(defvar iedit-printable-test-lists
  '(("" "")
    ("abc" "abc")
    ("abc
bcd" "abc...")
    ("abc\n34" "abc...")
    ("12345678901234567890123456789012345678901234567890abc" "12345678901234567890123456789012345678901234567890...")
    ("12345678901234567890123456789012345678901234567890abc
abcd" "12345678901234567890123456789012345678901234567890...")))

(ert-deftest iedit-printable-test ()
  (dolist (test iedit-printable-test-lists)
    (should (string= (iedit-printable (car test)) (cadr test)))))


;;; iedit-tests.el ends here

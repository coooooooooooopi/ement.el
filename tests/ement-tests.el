;;; ement-tests.el --- Tests for Ement.el                  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'ert)
(require 'map)

(require 'ement-lib)
(require 'ement-room)

;;;; Tests

(ert-deftest ement--format-body-mentions ()
  (let ((room (make-ement-room
               :members (map-into
                         `(("@foo:matrix.org" . ,(make-ement-user :id "@foo:matrix.org"
                                                                  :displayname "foo"))
                           ("@bar:matrix.org" . ,(make-ement-user :id "@bar:matrix.org"
                                                                  :displayname "bar")))
                         '(hash-table :test equal)))))
    (should (equal (ement--format-body-mentions "@foo: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: hi"))
    (should (equal (ement--format-body-mentions "@foo:matrix.org: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: hi"))
    (should (equal (ement--format-body-mentions "foo: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: hi"))
    (should (equal (ement--format-body-mentions "@foo and @bar:matrix.org: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a> and <a href=\"https://matrix.to/#/@bar:matrix.org\">bar</a>: hi"))
    (should (equal (ement--format-body-mentions "foo: how about you and @bar ..." room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: how about you and <a href=\"https://matrix.to/#/@bar:matrix.org\">bar</a> ..."))
    (should (equal (ement--format-body-mentions "Hello, @foo:matrix.org." room)
                   "Hello, <a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>."))
    (should (equal (ement--format-body-mentions "Hello, @foo:matrix.org, how are you?" room)
                   "Hello, <a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>, how are you?"))))

;;; Spoilers

(ert-deftest ement-room--org-spoiler-fallback ()
  (should
   (equal "~[[spoiler:][literal]]~ [Spoiler]"
          (ement-room--org-spoiler-fallback
           "~[[spoiler:][literal]]~ [[spoiler:][hidden]]"))))

(ert-deftest ement-room-send-org-filter-spoiler ()
  (let* ((room (make-ement-room))
         (content (ement-alist
                   "msgtype" "m.text"
                   "body" (concat "before [[spoiler:][plain]] and "
                                  "[[spoiler:a \\] & \" reason][*secret*]] after")))
         (filtered (ement-room-send-org-filter content room))
         (formatted-body (alist-get "formatted_body" filtered nil nil #'equal)))
    (should (equal "before [Spoiler] and [Spoiler: a ] & \" reason] after"
                   (alist-get "body" filtered nil nil #'equal)))
    (should (string-match-p
             (regexp-quote "<span data-mx-spoiler=\"\">plain</span>")
             formatted-body))
    (should
     (string-match-p
      (regexp-quote
       "<span data-mx-spoiler=\"a ] &amp; &quot; reason\"><b>secret</b></span>")
      formatted-body))))

(ert-deftest ement-room--normalize-spoiler-attributes ()
  (with-temp-buffer
    (insert
     (concat "<span title=\"foo data-mx-spoiler bar\">visible</span>"
             "<span title='data-mx-spoiler' "
             "data-mx-spoiler>hidden</span>"
             "<span data-mx-spoiler = \"reason\">reasoned</span>"))
    (ement-room--normalize-spoiler-attributes)
    (should
     (equal
      (concat "<span title=\"foo data-mx-spoiler bar\">visible</span>"
              "<span title='data-mx-spoiler' "
              "data-mx-spoiler=\"\">hidden</span>"
              "<span data-mx-spoiler = \"reason\">reasoned</span>")
      (buffer-string)))))

(ert-deftest ement-room--render-spoiler ()
  ;; Spoiler normalization must not disturb ordinary HTML rendering.
  (should (equal "visible" (ement-room--render-html "<p>visible</p>")))
  ;; Rich contents keep their original buttons after revelation.
  (let ((rendered
         (ement-room--render-html
          (concat "<span data-mx-spoiler><strong>"
                  "<a href=\"https://example.org\">secret</a>"
                  "</strong></span>"))))
    (with-temp-buffer
      (insert rendered)
      (goto-char (point-min))
      (search-forward "secret")
      (let* ((pos (- (point) (length "secret")))
             (button (button-at pos))
             (mask (get-text-property pos 'display)))
        (should button)
        (should (stringp mask))
        (should (string-blank-p (substring-no-properties mask)))
        (should (equal "https://example.org" (get-text-property pos 'shr-url)))
        (ement-room--reveal-spoiler button)
        (should-not (get-text-property pos 'display))
        (should (equal "https://example.org" (get-text-property pos 'shr-url)))
        (should (button-at pos)))))
  ;; Plain revealed contents remain a button so the spoiler can be hidden again.
  (let ((rendered
         (ement-room--render-html
          "<span data-mx-spoiler=\"reason\">plain secret</span>")))
    (with-temp-buffer
      (insert rendered)
      (goto-char (point-min))
      (let* ((button (button-at (point)))
             (mask (get-text-property (point) 'display))
             (reason-pos (string-match-p (regexp-quote "reason") mask)))
        (should button)
        (should reason-pos)
        (should-not (equal (get-text-property 0 'face mask)
                           (get-text-property reason-pos 'face mask)))
        ;; Exercise normal button activation so button callback arguments are tested.
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (button-activate button)
        (should-not (buffer-modified-p))
        (should-not (get-text-property (point) 'display))
        (let ((rehide-button (button-at (point))))
          (should rehide-button)
          (button-activate rehide-button)
          (should-not (buffer-modified-p))
          (should (stringp (get-text-property (point) 'display))))))))

(ert-deftest ement-room--render-nested-spoiler ()
  (let ((rendered
         (ement-room--render-html
          (concat "<span data-mx-spoiler=\"outer\">outer "
                  "<span data-mx-spoiler=\"inner\">inner</span></span>"))))
    (with-temp-buffer
      (insert rendered)
      (goto-char (point-min))
      (search-forward "inner")
      (let* ((inner-pos (- (point) (length "inner")))
             (outer-button (button-at (point-min))))
        (should (string-match-p
                 (rx "outer")
                 (substring-no-properties (get-text-property inner-pos 'display))))
        (ement-room--reveal-spoiler outer-button)
        (let ((inner-mask (get-text-property inner-pos 'display)))
          (should (stringp inner-mask))
          (should (string-match-p (rx "inner") (substring-no-properties inner-mask)))
          (let ((inner-button (button-at inner-pos)))
            (should inner-button)
            (ement-room--reveal-spoiler inner-button)
            (should-not (get-text-property inner-pos 'display))))))))

(provide 'ement-tests)

;;; ement-tests.el ends here

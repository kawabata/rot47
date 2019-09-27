;;; rot47.el --- display a buffer in ROT47.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Taichi Kawabata

;; Author: Taichi Kawabata
;; Keywords: tools

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

;; Ceaser cipher encrypt/decrypt for Japanese (or any other ISO-2022
;; 94x94 character set) text, extended and compatible with rot13.

;; For characters that are not rotatable, it will remain unciphered.

;;; Code:

(defvar rot47-charset 'japanese-jisx0213.2004-1
  "Character set to be used for ROT47.")

(defvar rot47-function #'rot47-decode-charset
  "Function to be used for ROT47.")

(defun rot47-decode-charset (row col)
  (logand #x10ffff
          (decode-char rot47-charset (+ (* 256 (+ row 32)) col 32))))

(defun rot47-make-table (table)
  (cl-loop for row from 1 to 47 do
    (cl-loop for col from 1 to 94 do
      (let* ((from-char (apply rot47-function (list row col)))
             (to-char   (apply rot47-function (list (1+ (mod (+ 46 row) 94))
                                                    (1+ (mod (+ 46 col) 94))))))
        (when (and from-char to-char)
          (aset table to-char   (vector from-char))
          (aset table from-char (vector to-char))))))
  (cl-loop for i from 0 to 26 do
    (aset table (+ i ?a) (vector (+ (% (+ i 13) 26) ?a)))
    (aset table (+ i ?A) (vector (+ (% (+ i 13) 26) ?A))))
  table)

(defvar rot47-display-table
  (rot47-make-table (make-display-table))
  "Char table for ROT47 display.")

(defvar rot47-translate-table
  (rot47-make-table (make-translation-table))
  "Char table for ROT47 translation.")

;;;###autoload
(defun rot47 (object &optional start end)
  "ROT47 encrypt OBJECT, a buffer or string.

See also `rot13'."
  (if (bufferp object)
      (with-current-buffer object
        (rot47-region start end))
    (rot47-string object)))

;;;###autoload
(defun rot47-string (string)
  "Return ROT47 encryption of STRING."
  (with-temp-buffer
    (insert string)
    (rot47-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun rot47-region (start end)
  "ROT47 encrypt the region between START and END in current buffer."
  (interactive "r")
  (translate-region start end rot47-translate-table))

;;;###autoload
(defun rot47-other-window ()
  "Display current buffer in ROT47 in another window.

See also `rot13-other-window'."
  (interactive)
  (let ((w (display-buffer (current-buffer) t)))
    (set-window-display-table w rot47-display-table)))

;;;###autoload
(defun toggle-rot47-mode ()
  "Toggle the use of ROT47 encoding for the current window."
  (interactive)
  (if (eq (window-display-table) rot47-display-table)
      (set-window-display-table (selected-window) nil)
    (if (null (window-display-table))
	(set-window-display-table (selected-window) rot47-display-table))))

(provide 'rot47)

;;; rot47.el ends here

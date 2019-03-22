;;; mirage --- WIP

;; Copyright (C) 2019- blue0513

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: blue0513
;; URL: https://github.com/blue0513/mirage
;; Version: 0.1.0

;;; Commentary:

;; Edit your init.el
;;
;; (require 'mirage)
;;

;;; Code:

(defface mirage-default-face '((t (:background "purple"))) nil)
(defvar mirage-overlays nil)

(defun mirage-put-one (symbol face &optional start end)
  (let* ((start-pos (if (null start) (match-beginning 0) start))
	 (end-pos (if (null end) (match-end 0) end))
	 (ov (make-overlay start-pos end-pos)))
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'symbol symbol)
    ov))

(defun mirage--delete (mirage)
  (let* ((buffer (overlay-buffer mirage))
	 (start (overlay-start mirage))
	 (end (overlay-end mirage))
	 (region-count (mirage--count-chars-region start end buffer))
	 (line-count (mirage--count-chars-line start buffer)))
    (if (> line-count region-count)
	(mirage--delete-region buffer start end)
      (mirage-delete-line buffer start))))

(defun mirage--count-chars-line (start &optional buffer)
  (let* ((target-buffer (if (null buffer) (current-buffer) buffer)))
    (with-current-buffer target-buffer
      (save-excursion
	(goto-char start)
	(- (mirage--count-chars-region
	    (progn (forward-visible-line 0) (point))
	    (progn (forward-visible-line 1) (point)))
	   1)))))

(defun mirage--delete-region (buffer start end)
  (with-current-buffer buffer
    (save-excursion
      (delete-region start end))))

(defun mirage-delete-line (buffer pos)
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (delete-region
       (progn (forward-visible-line 0) (point))
       (progn (forward-visible-line 1) (point))))))

(defun mirage--count-chars-region (beg end &optional buffer)
  (let* ((target-buffer (if (null buffer) (current-buffer) buffer)))
    (with-current-buffer target-buffer
      (save-excursion
	(goto-char beg)
	(let* ((char-count (- end beg)))
	  char-count)))))

(defun mirage-on-point ()
  (push (mirage-put-one (thing-at-point 'symbol) 'mirage-default-face)
	mirage-overlays))

(defun mirage-on-region (beg end)
  (let* ((text (buffer-substring beg end)))
    (push (mirage-put-one text 'mirage-default-face beg end)
	  mirage-overlays)))

;; Main Functions

(defun mirage-off-all ()
  (interactive)
  (mapc #'mirage--delete mirage-overlays)
  (setq mirage-overlays nil))

(defun mirage-on ()
  (interactive)
  (if (region-active-p)
      (mirage-on-region (region-beginning) (region-end))
    (mirage-on-point)))

;; * provide

(provide 'mirage)

;;; mirage.el ends here

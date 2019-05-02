;;; mediawiki-mode.el --- a major mode for editing mediawiki  -*- lexical-binding: t -*-

;; Copyright (c) 2019 gruenerBogen

;; Author: gruenerBogen <GoleoBaer@web.de>
;; Keywords: MediaWiki-Mode
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comentary:

;; This major mode enables a more comfortable editing of MediaWiki in Emacs.
;; It is designed for the "Mathe für Nicht-Freaks" Wikibooks project and is
;; optimised for the use with this. However most of the functionality and
;; highlighting will probably still work for other purposes involving MediaWiki.

;;; Code:

(require 'font-lock)

(defface mediawiki-headings
  '((t (:inherit bold)))
  "Face for headings")
(defface mediawiki-italic
  '((t (:inherit italic)))
  "Face for italic emphasis")
(defface mediawiki-bold
  '((t (:inherit bold)))
  "Face for bold emphasis")
(defface mediawiki-bold-italic
  '((t (:inherit bold-italic)))
  "Face for bold-italic emphasis")

;; TODO This code doesn't match if we're inside an opening or closing math tag.
;; We should fix this.
(defun mediawiki-match-math ()
  "Checks if the cursor is inside a math environment. If this is the case,
it returns the beginning and ending position of that math environment as
CONS-cell. Otherwise returns NIL"
  ;; We start by assuming that we're in math mode
  (let ((not-found t) (in-math-mode t) math-begin math-end)
    ;; Find starting math tag
    (save-excursion
      (while not-found
	;; Find openig or closing math tag
	(when (looking-back "<\\(/?\\)math>.*")
	  (setq not-found nil)
	  ;; Check if we found a closing math tag
	  (if (string= (match-string-no-properties 1) "/")
	      (setq in-math-mode nil)
	    (setq math-begin (match-beginning 0))))
	;; After first line were looking from the back of previous line
	;; Check that we don't hit the beginning of the buffer
	(when (/= (forward-line -1) 0)
	  (setq not-found nil)
	  (setq in-math-mode nil))
	(end-of-line)))
    (when in-math-mode
      (save-excursion
	(setf not-found t)
	(while not-found
	  ;; Find closing math tag
	  (when (looking-at ".*?</math>")
	    (setq math-end (match-end 0))
	    (setq not-found nil))
	  ;; Check that we don't hit the end of the buffer
	  (when (and (/= (forward-line) 0) not-found)
	    (setq not-found nil)
	    (setq math-end nil))
	  (beginning-of-line)))
      (cons math-begin math-end))))

(defun mediawiki-font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "<math>" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "</math>" nil t)
	(setq font-lock-end (point)))
      (setq font-lock-begin found))))

(defun mediawiki-remove-math-tags ()
  "Remove math environment the cursor is inside while keeping the content of the math environment"
  (interactive)
  (let ((region (mediawiki-match-math)))
    (if region
	(progn
	  ;; Remove end tag first if existent
	  (when (cdr region)
	    (delete-region (- (cdr region) (length "</math>")) (cdr region)))
	  ;; Then remove beginning tag
	  (delete-region (car region) (+ (car region) (length "<math>"))))
      (message "You're not inside a math environment. Therefore I don't know what to remove."))))

;; TODO enter math editing mode if the cursor is inside the math environment, or after
;; the math environment has been created.
(defun mediawiki-insert-math-tags ()
  "Add math environment around cursor or region when the cursor isn't in a math environment."""
  (interactive)
  (let ((start (point)) (end (if (use-region-p)
				 (mark)
			       (point))))
    ;; Ensure start <= end
    (when (> start end)
      (let ((tmp start))
	(setq start end)
	(setq end tmp)))
    (if (mediawiki-match-math)
	(message "You're already inside a math environment. Thus adding another pair of math tags seems inappropriate.")
      (save-excursion
	(goto-char end)
	(insert "</math>")
	(goto-char start)
	(insert "<math>")))
    ;; When the cursor is before the opening math tag, move it inside the math environment.
    (when (>= start (point))
	(forward-char 6))))

(defvar mediawiki-font-lock-defaults
  `((("==+[^=|\n]+==+" . 'mediawiki-headings)
     ("{{#invoke[^=<>\n#}]+}}" . 'font-lock-keyword-face)
     ("{{[^=<>|\n#]+\\(|\\|\n\\)" . 'font-lock-keyword-face)
     ("|[^=<>|\n]+=" . 'font-lock-builtin-face)
     ("<math>\\(.\\|\n\\)*?</math>" . 'font-lock-variable-name-face)
     ("'''''[^'|\n]+'''''" . 'mediawiki-bold-italic)
     ("'''[^'|\n]+'''" . 'mediawiki-bold)
     ("''[^'|\n]+''" . 'mediawiki-italic))))


(define-derived-mode mediawiki-mode text-mode "MediaWiki"
  "Major mode for editing MediaWiki files."
  ;; Basic font lock
  (setq font-lock-defaults mediawiki-font-lock-defaults)
  ;; Enable multiline font-lock
  (setq font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions
	    'mediawiki-font-lock-extend-region)
  (define-key mediawiki-mode-map (kbd "C-c C-r C-m")
    'mediawiki-remove-math-tags)
  (define-key mediawiki-mode-map (kbd "C-c C-m")
    'mediawiki-insert-math-tags))

(provide 'mediawiki-mode)

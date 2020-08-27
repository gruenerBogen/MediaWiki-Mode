;;; mediawiki-mode.el --- a major mode for editing mediawiki  -*- lexical-binding: t -*-

;; Copyright (c) 2019-2020 gruenerBogen

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

;;; Commentary:

;; This major mode enables a more comfortable editing of MediaWiki in Emacs.
;; It is designed for the "Mathe für Nicht-Freaks" Wikibooks project and is
;; optimised for the use with this.  However most of the functionality and
;; highlighting will probably still work for other purposes involving MediaWiki.

;;; Code:

(require 'font-lock)

;;; Customisations

(defgroup mediawiki nil
  "Major mode for editing MediaWiki files."
  :group 'text
  :prefix "mediawiki-")

(defface mediawiki-headings
  '((t (:inherit bold)))
  "Face for headings"
  :group 'mediawiki)
(defface mediawiki-italic
  '((t (:inherit italic)))
  "Face for italic emphasis"
  :group 'mediawiki)
(defface mediawiki-bold
  '((t (:inherit bold)))
  "Face for bold emphasis"
  :group 'mediawiki)
(defface mediawiki-bold-italic
  '((t (:inherit bold-italic)))
  "Face for bold-italic emphasis"
  :group 'mediawiki)
(defface mediawiki-link-face
  '((t (:underline t :foreground "RoyalBlue1")))
  "Face for links."
  :group 'mediawiki)

;; TODO This code doesn't match if we're inside an opening or closing math tag.
;; We should fix this.
(defun mediawiki-match-math ()
  "Check if the cursor is inside a math environment.

If this is the case, return the beginning and ending position of that math
environment as CONS-cell.  Otherwise return NIL."
  ;; We start by assuming that we're in math mode
  (let ((not-found t) (in-math-mode t) math-begin math-end)
    ;; Find starting math tag
    (save-excursion
      (while not-found
	;; Find openig or closing math tag
	(when (looking-back "<\\(/?\\)math>.*" nil)
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

;;; Insertion and deletion of tags

(defvar mediawiki-inserted-tag-history '("")
  "The history of tags entered in mediawiki-insert-tag.")
(defvar mediawiki-tag-list '("nowiki" "math" "pre")
  "The list of (initial) tags completed by mediawiki-insert-tag.")

(defun mediawiki-insert-text-arount-region (before-text after-text)
  "Insert BEFORE-TEXT before and AFTER-TEXT after the current region.

If the current region is not active, insert BEFORE-TEXT before and AFTER-TEXT
after the point."
  (let ((start (point)) (end (if (use-region-p)
				 (mark)
			       (point))))
    ;; Ensure start <= end
    (when (> start end)
      (let ((tmp start))
	(setq start end)
	(setq end tmp)))
      (save-excursion
	(goto-char end)
	(insert after-text)
	(goto-char start)
	(insert before-text))
      ;; When the point is before the opening tag, move it inside the tag environment.
      (when (>= start (point))
	(forward-char (length before-text)))))

(defun mediawiki-insert-tag (tag-name)
  "Insert TAG-NAME around the current region.

If the current region is not active, insert TAG-NAME around the point."
  ;; If the command is called interactively, prompt for the tag name in the
  ;; minibuffer.
  (interactive
   (list (completing-read (format "Tag name [%s]: " (car mediawiki-inserted-tag-history))
			  (append mediawiki-tag-list mediawiki-inserted-tag-history)
			  nil nil ""
			  'mediawiki-inserted-tag-history
			  (car mediawiki-inserted-tag-history))))
  ;; Get the region if it is active.
  (mediawiki-insert-text-arount-region (format "<%s>" tag-name)
				       (format "</%s>" tag-name)))

;; TODO enter math editing mode if the cursor is inside the math environment, or after
;; the math environment has been created.
(defun mediawiki-insert-math-tags ()
  "Add math environment around point or region when the point isn't in a math environment."
  (interactive)
  (if (mediawiki-match-math)
      (message "You're already inside a math environment. Thus adding another pair of math tags seems inappropriate.")
    (mediawiki-insert-tag "math")))

(defun mediawiki-remove-math-tags ()
  "Remove math environment the cursor is inside while keeping the content of the math environment."
  (interactive)
  (let ((region (mediawiki-match-math)))
    (if region
	(progn
	  ;; Remove end tag first if existent
	  (when (cdr region)
	    (delete-region (- (cdr region)
			      (length "</math>"))
			   (cdr region)))
	  ;; Then remove beginning tag
	  (delete-region (car region) (+ (car region) (length "<math>"))))
      (message "You're not inside a math environment. Therefore I don't know what to remove."))))

;;; Commands for changing font properties

(defun mediawiki-insert-bold-tag ()
  "Insert bold tags around the region or the point if the region is inactive."
  (interactive)
  (mediawiki-insert-text-arount-region "'''" "'''"))

(defun mediawiki-insert-italic-tag ()
  "Insert italic tags around the rgion or the point if the region is inactive."
  (interactive)
  (mediawiki-insert-text-arount-region "''" "''"))

;;; Insertion of sections

(defvar mediawiki-inserted-section-depth-history '("1")
  "The section depth entered in the last mediawiki-isert-section call.")

(defun mediawiki-insert-section ()
  "Add a section at the cursor with a specified depth."
  (interactive)
  (let ((section-depth
	 (string-to-number
	  (completing-read (format "Section Level [%s]: "
				   (first mediawiki-inserted-section-depth-history))
			   mediawiki-inserted-section-depth-history
			   nil nil nil
			   'mediawiki-inserted-section-depth-history
			   (first mediawiki-inserted-section-depth-history))))
	(section-title (read-string "Tile: ")))
    (let ((section-mark (make-string (+ section-depth 1) ?=)))
      (insert (format "%s %s %s\n" section-mark section-title section-mark))
      (first mediawiki-inserted-section-depth-history))))

(defun mediawiki-continue-or-insert-list-item ()
  "Continue an (un-)ordered list by inserting the notation for a new list item in a new line."
  (interactive)
  (let ((inserted-character ?*))
    (save-excursion
      (beginning-of-line)
      (if (eql (char-after) ?#)
	  (setq inserted-character ?#)))
    (insert (format "\n%c " inserted-character))))

(defun mediawiki-insert-link (link-pattern &optional destination-history)
  "Ask for destination and link name and insert both according to LINK-PATTERN.

The default for the link name will be the destination.
LINK-PATTERN is a string according using the rules of the format command.  The
first parameter will be the destination and the second the link name.
DESTINATION-HISTORY is a list which contains the history of destinations.

This returns the destination which was chosen by the user."
  (if (not destination-history)
      (setf destination-history '("")))
  (let ((destination
	 (completing-read (format "Destination [%s]: " (car destination-history))
			  destination-history
			  nil nil ""
			  nil
			  (car destination-history))))
    (let ((link-name
	   (completing-read (format "Link name [%s]: " destination)
			    (list destination)
			    nil nil ""
			    nil
			    destination)))
      (insert (format link-pattern destination link-name)))
    destination))

(defmacro mediawiki-prepend-to-list-if-not-first (list-to-add-to value &optional predicate)
  "Add VALUE to the list LIST-TO-ADD-TO if it is not the first value on it.

PREDICATE is used for the comparison.  It defaults to eq."
  (if (not predicate)
      (setf predicate 'eq))
  (let ((value-storage (gensym)))
    `(let ((,value-storage ,value))
       (if (not (,predicate ,value-storage (car ,list-to-add-to)))
	   (setq ,list-to-add-to
		 (cons ,value-storage ,list-to-add-to))))))

(defvar mediawiki-inserted-internal-link-destination-history '("")
  "The history of destinations of internal links entered in mediawiki-insert-internal-link.")

(defun mediawiki-insert-internal-link ()
  "Insert an internal link using the questions of mediawiki-insert-link."
  (interactive)
  (mediawiki-prepend-to-list-if-not-first
   mediawiki-inserted-internal-link-destination-history
   (mediawiki-insert-link "[[%1$s|%2$s]]"
			  mediawiki-inserted-internal-link-destination-history)
   string=))

(defvar mediawiki-inserted-external-link-destination-history '("")
  "The history of destinations of external links entered in mediawiki-insert-external-link.")

(defun mediawiki-insert-external-link ()
  "Insert an external link using the questions of mediawiki-insert-link."
  (interactive)
  (mediawiki-prepend-to-list-if-not-first
   mediawiki-inserted-external-link-destination-history
   (mediawiki-insert-link "[%1$s %2$s]"
			  mediawiki-inserted-external-link-destination-history)
   string=))

(defun mediawiki-copy-for-wikibooks ()
  "Copy the entire buffer's text to the clipboard.

This is for manually uploading your edits to wikibooks."
  (interactive)
  (kill-new (buffer-substring-no-properties (buffer-end -1) (buffer-end 1)))
  (message "Copied the entire buffer, such that you can paste it into the wikibooks editor."))

;;; Indentation

(defun mediawiki-template-argument-p (&optional n)
  "Check if the current line starts with a template argument.

Returns non-nil if the current line starts with a template argument and nil
otherwise.  A line is said to start with a template argument if the line starts
with \"[:space:]+|[:word:]+=\".

The parameter N is used as in `beginning-of-line'."
  (save-excursion
    (beginning-of-line n)
    (re-search-forward "\\=[[:blank:]]*|\\sw+=" (line-end-position) t)))

(defun mediawiki-indentation-of-line (&optional n)
  "Return indentation of the current line.

The parameter N is used as in `beginning-of-line'."
  (save-excursion
    (beginning-of-line n)
    (re-search-forward "\\=\\([[:blank:]]*\\)" (line-end-position))
    (match-string 1)))

(defun mediawiki-indent-line-to (&optional n)
  "Indent current line such that it begins with N spaces.

If N is nil, it defaults to 0."
  (when (not n)
    (setf n 0))
  (let ((desired-indentation (make-string n ?\s)))
    ;; Only change indentation, when the indentation is wrong.
    (unless (string= desired-indentation (mediawiki-indentation-of-line))
      (save-excursion
	(beginning-of-line)
	;; Remove all present spaces
	(while (or (char-equal (char-after) ?\s) (char-equal (char-after) ?\t))
	  (delete-char 1))
	;; Insert desired indentation
	(insert desired-indentation)))))

(defun mediawiki-point-to-end-of-indentation ()
  "Move the poitn to the end of indentation, if the point is inside the indentation."
  (let ((indent-end (+ (line-beginning-position)
		       (length (mediawiki-indentation-of-line)))))
    (when (< (point) indent-end)
      (goto-char indent-end))))

(defun mediawiki-indent-line ()
  "Indent the current line according in MediaWiki-mode.

Currently this does the following (only the first applicable case is executed):
1. If the current line starts with a template argument according to
   `mediawiki-template-argument-p', the line is treated as an argument for a
   template.  These are indented by a single space.
2. If the previous line falls into case 1. no indentation is allowed.
3. Indent in the same way as the previous line.  (That is, if the indentation of
   the previous line and the current line differ, indent the same amount as the
   previous line.)"
  (interactive)
  ;; Implementation of case 1.
  (if (mediawiki-template-argument-p)
      (mediawiki-indent-line-to 1)
    ;; Implementation of case 2.
    (if (mediawiki-template-argument-p 0)
	(mediawiki-indent-line-to 0)
      ;; Implementation of case 3.
      (let ((previous-indentation (mediawiki-indentation-of-line 0)))
	(unless (string= (mediawiki-indentation-of-line)
			 previous-indentation)
	  ;; Remove old indentation
	  (mediawiki-indent-line-to 0)
	  ;; Insert indentation ofprevious line.
	  (save-excursion
	    (beginning-of-line)
	    (insert previous-indentation))))))
  (mediawiki-point-to-end-of-indentation))

;;; Font-lock setup

(defvar mediawiki-font-lock-defaults
  `((("==+[^=|\n]+==+" . 'mediawiki-headings)
     ("{{#invoke[^=<>\n#}]+}}" . 'font-lock-keyword-face)
     ("{{[^=<>|\n#]+\\(|\\|\n\\)" . 'font-lock-keyword-face)
     ("|[^=<>|\n]+=" . 'font-lock-builtin-face)
     ("<math>\\(.\\|\n\\)*?</math>" . 'font-lock-variable-name-face)
     ("'''''[^'|\n]+'''''" . 'mediawiki-bold-italic)
     ("'''[^'|\n]+'''" . 'mediawiki-bold)
     ("''[^'|\n]+''" . 'mediawiki-italic)
     ("\\[\\[[^|]+|[^]]+\\]\\]" . 'mediawiki-link-face)
     ("\\[[^][[:space:]]+://[^][[:space:]]+ [^]]+\\]" . 'mediawiki-link-face))))

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

;;; Mode definition

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
    'mediawiki-insert-math-tags)
  (define-key mediawiki-mode-map (kbd "C-c C-s")
    'mediawiki-insert-section)
  (define-key mediawiki-mode-map (kbd "C-c m")
    'mediawiki-insert-tag)
  (define-key mediawiki-mode-map (kbd "C-c <return>")
    'mediawiki-insert-tag)
  (define-key mediawiki-mode-map (kbd "M-RET")
    'mediawiki-continue-or-insert-list-item)
  (define-key mediawiki-mode-map (kbd "C-c C-u")
    'mediawiki-copy-for-wikibooks)
  (define-key mediawiki-mode-map (kbd "C-c )")
    'mediawiki-insert-internal-link)
  (define-key mediawiki-mode-map (kbd "C-c [")
    'mediawiki-insert-external-link)
  ;; Keys for changing font
  (define-key mediawiki-mode-map (kbd "C-c C-f C-b")
    'mediawiki-insert-bold-tag)
  (define-key mediawiki-mode-map (kbd "C-c C-f C-i")
    'mediawiki-insert-italic-tag)
  (define-key mediawiki-mode-map (kbd "C-C C-f C-e")
    'mediawiki-insert-italic-tag)
  ;; Disable auto fill and enable visual line mode instead. This prevents
  ;; automated line breaks while still maintaining a readable text.
  (add-hook 'mediawiki-mode-hook 'turn-off-auto-fill)
  (add-hook 'mediawiki-mode-hook 'visual-line-mode)
  ;; Setup indentation with mediawiki-indent-line
  (set (make-local-variable 'indent-line-function) 'mediawiki-indent-line))

;; Set .mw as file ending for MediaWiki-Mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mw\\'" . mediawiki-mode))

(provide 'mediawiki-mode)
;;; mediawiki-mode.el ends here

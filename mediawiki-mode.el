(require 'font-lock)

(defface mediawiki-headings
  '((t (:inherit bold)))
  "Face for headings")

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

(defvar mediawiki-font-lock-defaults
  `((("==+[^=|\n]+==+" . 'mediawiki-headings)
     ("{{#invoke[^=<>\n#}]+}}" . 'font-lock-keyword-face)
     ("{{[^=<>|\n#]+\\(|\\|\n\\)" . 'font-lock-keyword-face)
     ("|[^=<>|\n]+=" . 'font-lock-builtin-face)
     ("<math>.*?</math>" . 'font-lock-variable-name-face))))

(define-derived-mode mediawiki-mode text-mode "MediaWiki"
  "Major mode for editing MediaWiki files."
  (setq font-lock-defaults mediawiki-font-lock-defaults))

(provide 'mediawiki-mode)

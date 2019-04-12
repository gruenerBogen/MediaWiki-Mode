(require 'font-lock)

(defface mediawiki-headings
  '((t (:inherit bold)))
  "Face for headings")

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

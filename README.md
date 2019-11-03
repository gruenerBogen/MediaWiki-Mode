# MediaWiki-Mode
A simple Emacs major mode for editing Wikibook's MediaWiki. This is optimised for https://de.wikibooks.org/wiki/Mathe_f%C3%BCr_Nicht-Freaks

# Installation
Download `mediawiki-mode.el` and put it somewhere you recognise. Then put the
following into your `.emacs`-file:
```lisp
(load-file "Path to mediawiki-mode.el")
```

This adds `.mw` as file ending for MediaWiki files.

# Command Reference
MediaWiki-Mode defines the following commands:

* Insert math tags: <kbd>C-c C-m</kbd>. If you highlighted a region, this
  inserts the math-tags around the region. Otherwise this inserts the
  math-tags around the cursor. This does nothing if the cursor is inside a
  maths environment.
* Remove math tags: <kbd>C-c C-r C-m</kbd>. This removes the math-tags if the
  cursor is inside a maths environment.
* Insert section heading: <kbd>C-c C-s</kbd>. This prompts for a section level,
  and a heading. Then it inserts the section at the point of the cursor.
* Insert pair of tags: <kbd>C-c RET</kbd>. This prompts for a tag to
  insert. This inserts an opening and closing tag. For example if you want to
  insert an nowiki-tag this inserts ```<nowiki></nowiki>```.

# Configuration
MediaWiki-Mode has the following variables for customisation:

* ```mediawiki-tag-list```: This is a list of strings containing all tags
  which will be completed when inserting tags. In addition to these all tags
  the tag insertion history (since opening emacs) will be completed.

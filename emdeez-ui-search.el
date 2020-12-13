;;; emdeez-ui-search.el --- Provides the search UI to emdeez  -*- lexical-binding: t -*-

;; Author: Thibaud Toullier
;; Maintainer: Thibaud Toullier
;; Version: 0.9.0-alpha
;; Package-Requires: ((all-the-icons))
;; URL: https://github.com/KirmTwinty/emdeez
;; Homepage: https://github.com/KirmTwinty/emdeez
;; Keywords: emacs, deezer, music, player

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:
(require 'emdeez-player)

(defcustom emdeez-search-frame-width 50
  "Emdeez-search frame width in number of char.")

(defcustom emdeez-search-frame-height 32
  "Emdeez-search frame height in pixels.")

(defcustom emdeez-search-ui-animate t
  "Either to perform animation on emdeez pop-up.")

(defvar emdeez-search-item-counter 0
  "A counter to keep track of current item.")

(defvar emdeez-search-result nil
  "Result of last search.")

(defface emdeez-item-1
  '((((class color) (min-colors 88) (background light))
     :background "#eee" :foreground "black")
    (((class color) (min-colors 88) (background dark))
     :background "#333" :foreground "white")
    (((class color) (min-colors 16) (background light))
     :background "#eee" :foreground "black")
    (((class color) (min-colors 16) (background dark))
     :background "#333" :foreground "white")
    (((class color) (min-colors 8))
     :background "white" :foreground "black")
    (t :inverse-video t))
  "Face for showing search results."
  :group 'emdeez)

(defface emdeez-item-2
  '((((class color) (min-colors 88) (background light))
     :background "#aaa" :foreground "black")
    (((class color) (min-colors 88) (background dark))
     :background "#444" :foreground "white")
    (((class color) (min-colors 16) (background light))
     :background "#aaa" :foreground "black")
    (((class color) (min-colors 16) (background dark))
     :background "#444" :foreground "white")
    (((class color) (min-colors 8))
     :background "white" :foreground "black")
    (t :inverse-video t))
  "Face for showing search results."
  :group 'emdeez)

(defun emdeez-ui-search-make-frame ()
  "Create the emdeez-ui-search popup frame."
  (interactive)
  (let* ((w (cons 'text-pixels 200))
	(h (cons 'text-pixels emdeez-search-frame-height))
	(emdeez-search-frame (make-frame '((minibuffer . nil)
					   (name . "emdeez-search")
					   (visibility . nil))))
	x y)
    (internal-show-cursor (frame-selected-window emdeez-search-frame) nil)
    (set-frame-size emdeez-search-frame 90 50)
    (set-frame-parameter emdeez-search-frame 'undecorated t)
    (with-selected-frame emdeez-search-frame
      (emdeez-ui-search-make-buffer)
      (switch-to-buffer "*emdeez-search*")
      (setq buffer-read-only nil)
      (use-local-map emdeez-search-mode-map)
      (setq mode-line-format nil))
    (emdeez-ui-show-frame emdeez-search-frame)))

(defun emdeez-ui-search-make-buffer ()
  "Renders the emacs-ui-search-buffer."
  (when (get-buffer "*emdeez-search*")
    (kill-buffer "*emdeez-search*")
    )
  (set-buffer (get-buffer-create "*emdeez-search*"))
  (setq header-line-format (concat 
			    "   " 
			    "Title" (make-string 38 ? ) 
			    "Artist" (make-string 10 ? )
			    "Album" (make-string 10 ? )
			    "Duration"))
  (insert (all-the-icons-faicon "search" :height 1))
  (insert " ")
  (save-excursion
    (newline)
    (insert (make-string 60 ?\u2501))
    (newline)
    (emdeez-ui-make-shortcuts-bar (cdr emdeez-search-mode-map))
    ))

(defun emdeez-ui-search-clear-buffer ()
  "Clear the Emdeez buffer."
  ;; Save the query
  (let ((query (emdeez-ui-search-get-query)))
    (erase-buffer)
    (beginning-of-buffer)
    (insert (all-the-icons-faicon "search" :height 1))
    (insert " ")
    (insert query)
    (save-excursion
      (newline))))

(defun emdeez-ui-search-get-query ()
  "Retrieve user's query from search field."
  (save-excursion
  (beginning-of-buffer)
  (substring (thing-at-point 'line t) 2 -1)))

(defun emdeez-ui-search-query ()
  "Perform a search query and displays it."
  (interactive)
  (with-current-buffer (get-buffer "*emdeez-search*")
    (let ((query (save-excursion
		   (beginning-of-buffer)
		   (substring (thing-at-point 'line t) 2 -1)
		   )))
      (emdeez-server-query emdeez-server query 'emdeez-ui-search-display-query)
      (emdeez-ui-search-clear-buffer)
      (save-excursion
	(newline)
	(insert (make-string 60 ?\u2501))
	(newline)
	(emdeez-ui-make-shortcuts-bar (cdr emdeez-search-mode-map))
	(newline)
	(newline)
	(insert "Searching...")
	))))

(defun emdeez-ui-search-display-query (query)
  "Display the query QUERY to the current buffer."
  (setq emdeez-search-result query)
  (with-current-buffer (get-buffer "*emdeez-search*")
    (emdeez-ui-search-clear-buffer)
    (save-excursion
      (newline)
      (insert (make-string 60 ?\u2501))
      (newline)
      (emdeez-ui-make-shortcuts-bar (cdr emdeez-search-mode-map))
      (newline)
      (setq emdeez-search-item-counter 0)
      (if query
	  (emdeez-ui-insert-playlist query 'emdeez-ui-search-click-item)
	(newline)
	(insert "Sorry, nothing found.")
	)
      )))
    ;;(emdeez-ui-insert-playlist (coerce query 'list) 'emdeez-ui-search-click-item)))
  

;; TODO here!!! action is button, see button-start for actual id or label  
(defun emdeez-ui-search-click-item (button)
  "Gathers the tracks search below the N-th item and create a new list to be played."
  (interactive)
  ;; Retrieve the selected item with the button position.
  (let* ((startPos (button-start button))
	 (itemNumber (- (floor (/ startPos 85)) 2))
	 (playlist (cl-coerce (seq-drop emdeez-search-result itemNumber) 'list)))
    (setq emdeez-player-playlist playlist)
    (emdeez-player-play-pause (mapcar (lambda (x) (alist-get 'id x)) playlist)))
  (emdeez-ui-search-quit))

(defun emdeez-utils-text-length (text len &optional alignRight)
  "Set a given string TEXT to LEN either trucate or fill with spaces.
   When ALIGNRIGHT is set to t text is padded from the left."
  (let ((currentLength (length text)))
    (if (< currentLength len)
	(if alignRight
	    (concat (make-string (- len currentLength) ? ) text)
	    (concat text (make-string (- len currentLength) ? ))
	  )
      (concat (substring text 0 (- len 3)) "...")
      )
    )
  )
(defun emdeez-ui-search-quit ()
  "Kill emacs music ui search window."
  (interactive)
  (delete-frame))

(provide 'emdeez-ui-search)

;;; emdeez-ui-search.el ends here

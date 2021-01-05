;;; emdeez-ui.el --- Provides the UI to emdeez -*- lexical-binding: t -*-

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

(require 'all-the-icons)
(require 'emdeez-ui-progress)

(defcustom emdeez-ui-alpha 90
  "Emdeez frame transparency (0-100).")
(defcustom emdeez-frame-width 500
  "Emdeez frame width in pixels.")
(defcustom emdeez-frame-height 10
  "Emdeez frame height in pixels.")
(defcustom emdeez-ui-animate t
  "Either to perform animation on emdeez pop-up.")
(defface emdeez-highlight
  '((t :inherit highlight))
  "Face for highlighting things.")

(defvar emdeez-ui-refresh-timer nil
  "Timer for refreshing the buffer.")

(defvar emdeez-ui-cursor-pos nil
  "Cursor position on the seek bar (nil otherwise) in emdeez player.")

(defun emdeez-ui-make-frame ()
  "Create the emdeez-ui popup frame."
  (interactive)
  (let* ((w (cons 'text-pixels 200))
	 (h (cons 'text-pixels emdeez-frame-height))
	 (emdeez-frame (make-frame '((minibuffer . nil)
				     (name . "emdeez")
				     (visibility . nil))))
	 x y)
    (set-frame-size emdeez-frame 92 50)
    (set-frame-parameter emdeez-frame 'undecorated t)
    (internal-show-cursor (frame-selected-window emdeez-frame) nil)
    (with-selected-frame emdeez-frame
      (when (get-buffer "*emdeez-playlist*")
	(kill-buffer "*emdeez-playlist*"))
      (switch-to-buffer "*emdeez-playlist*")
      (use-local-map emdeez-mode-map)
      (setq mode-line-format nil)
      (split-window nil 18 nil)
      (when (get-buffer "*emdeez-image*")
	(kill-buffer "*emdeez-image*"))
      (switch-to-buffer "*emdeez-image*")
      (use-local-map emdeez-mode-map)
      (setq mode-line-format nil)
      (split-window nil 60 t)
      (when (get-buffer "*emdeez*")
	(kill-buffer "*emdeez*"))
      (switch-to-buffer "*emdeez*")
      (use-local-map emdeez-mode-map)
      (add-hook 'post-command-hook #'emdeez-ui-motion nil :local)
      (setq mode-line-format nil)
      (emdeez-ui-update-buffers))
    (emdeez-ui-show-frame emdeez-frame))
  ;; Ask to refresh data from distant server
  (emdeez-refresh)
  ;; start the timer
  (setq emdeez-ui-refresh-timer (run-at-time "1 sec" 0.5 'emdeez-ui-update-buffers)))

(defun emdeez-ui-show-frame (frame)
  "Show the FRAME on screen.  
If `emdeez-ui-animate' is set to t, the appears smoothly."  
  (if emdeez-ui-animate
      (progn
	(let ((x (floor (/ (- (window-pixel-width (frame-root-window)) emdeez-frame-width) 2)))
	      (y 50)
	      (alpha 0))
	  (set-frame-position frame x y)
	  (set-frame-parameter frame 'alpha `(,alpha ,alpha))
	  (make-frame-visible frame)
	(emdeez-ui-do-animation frame x y alpha)))
    (setq x (floor (/ (- (window-pixel-width (frame-root-window)) emdeez-frame-width) 2)))
    (setq y 50)
    (set-frame-position frame x y)
    (make-frame-visible frame)))

(defun emdeez-ui-do-animation (frame x y alpha)
  "Perform animation on FRAME at pos X and Y with ALPHA transparency."
  (message (concat (number-to-string x) ", " (number-to-string y)))
  (when (< alpha emdeez-ui-alpha)
    (progn (set-frame-parameter frame 'alpha `(,alpha ,alpha))
	   (sit-for 0.1)
	   (emdeez-ui-do-animation frame x y (+ alpha 5)))))

(defun emdeez-ui-update-buffers ()
  "Renders the emdeez playlist and main buffers."  
  (with-current-buffer (get-buffer-create "*emdeez*")
    (let ((cursor (point)))
      (nlinum-mode -1)
      (setq buffer-read-only nil)
      (erase-buffer)
      (emdeez-ui-insert-player)
      (newline)
      (emdeez-ui-make-shortcuts-bar (cdr emdeez-mode-map))
      (setq buffer-read-only t)
      (goto-char cursor))
    )
  (with-current-buffer (get-buffer-create "*emdeez-playlist*")
    (let ((cursor (point)))
      (setq header-line-format (concat 
			        (make-string 11 ? )
				"Title" (make-string 32 ? ) 
				"Artist" (make-string 10 ? )
				"Album" (make-string 16 ? )
				"Duration"))
      (erase-buffer)
      (emdeez-ui-insert-playlist emdeez-player-playlist 'emdeez-ui-play-this)
      (goto-char cursor))
    )
  (with-current-buffer (get-buffer-create "*emdeez-image*")
    (let ((cursor (point)))
      (nlinum-mode -1)
      (setq buffer-read-only nil)
      (erase-buffer)
      (when emdeez-player-playlist
	  (let ((currentTrack (nth (alist-get 'number emdeez-player-current-track) emdeez-player-playlist)))
	    (insert-image (emdeez-get-url-image (alist-get 'cover_medium (alist-get 'album currentTrack))))))
      (setq buffer-read-only t)
      (goto-char cursor))
    )
  
  )

;; (defun emdeez-ui-update-buffer ()
;;   "Renders the emacs-ui-buffer with data in USER."  
;;   (set-buffer (get-buffer-create "*emdeez*"))
;;   ;; Due to erase-buffer cannot use save-excursion
;;   (let ((cursor (point)))
;;     (setq buffer-read-only nil)
;;     (erase-buffer)
;;     (emdeez-ui-insert-player)
;;     (newline)
;;     (emdeez-ui-insert-playlist emdeez-player-playlist)
;;     (newline)
;;     (emdeez-ui-make-shortcuts-bar (cdr emdeez-mode-map))
;;     (setq buffer-read-only t)
;;     (goto-char cursor)))



(defun emdeez-ui-insert-player ()
  "Insert the player UI into current buffer."
  (if emdeez-player-playlist
      (let ((currentTrack (nth (alist-get 'number emdeez-player-current-track) emdeez-player-playlist)))
	(insert "  ")
	(insert (all-the-icons-faicon "step-backward" :height 2.2 ))
	(insert "   ")
	(if (eq emdeez-player-status 0)
	    (insert (all-the-icons-faicon "play" :height 3.4 ))
	  (insert (all-the-icons-faicon "pause" :height 3.4 )))
	(insert "   ")
	(insert (all-the-icons-faicon "step-forward" :height 2.2 ))
	(insert (make-string 23 ? ))	
	(newline)
	(let ((currentTime 
	       (if (= emdeez-player-status 0)
		   (alist-get 'time-paused emdeez-player-current-track)
		 (float-time (time-subtract (float-time) (alist-get 'time-started emdeez-player-current-track)))))
	      (duration (alist-get 'duration currentTrack)))
	  (if (<= currentTime duration)
	      (emdeez-ui-progress-update currentTime 0 duration)
	    (progn  ;; Next or previous track probably, ask for update
	      (emdeez-ui-progress-update 0 0 duration)
	      (emdeez-refresh))))
	(newline)
	(insert (propertize (alist-get 'title currentTrack)
			    'face '(:height 1.6) 
			    'display '(raise -0.5)))
	(newline)
	(insert (propertize (alist-get 'title (alist-get 'album currentTrack))
			    'face '(:height 1.2) 
			    'display '(raise -0.1)))
	)
    (insert "  ")
    (insert (all-the-icons-faicon "step-backward" :height 2.2 ))
    (insert "   ")
    (insert (all-the-icons-faicon "play" :height 3.4 ))
    (insert "   ")
    (insert (all-the-icons-faicon "step-forward" :height 2.2 ))
    (insert (make-string 23 ? ))
    (newline)
    (emdeez-ui-progress-update 10 0 137)
    (newline)
    (insert (propertize "Title of the song - it can be long" 
			'face '(:height 1.6) 
			'display '(raise -0.5)))
    (newline)
    (insert (propertize "Album" 
			'face '(:height 1.2) 
			'display '(raise -0.1)))))

(defun emdeez-ui-insert-playlist (items action)
  "Display the different track of the playlist ITEMS to the current buffer
   When clicking on an item ACTION button callback is executed."
  (when (car items)
    (let* ((item (car items))
	   (fontface (if (and emdeez-player-playlist emdeez-player-current-track)
			 (if (eq (alist-get 'id (nth (alist-get 'number emdeez-player-current-track) emdeez-player-playlist)) (alist-get 'id item))
			     'emdeez-highlight
			   (if (eq (mod (line-number-at-pos) 2) 0)
			       'emdeez-item-1
			     'emdeez-item-2
			     ))
		       (if (eq (mod (line-number-at-pos) 2) 0)
			   'emdeez-item-1
			 'emdeez-item-2
			 ))))
      (insert-image (emdeez-get-url-image (alist-get 'cover_small (alist-get 'album item))))
      (insert (concat 
	       " "
	       (emdeez-utils-text-length (alist-get 'title item) 36) " "
	       (emdeez-utils-text-length (alist-get 'name (alist-get 'artist item)) 15) " "
	       (emdeez-utils-text-length (alist-get 'title (alist-get 'album item)) 20) " "
	       (emdeez-utils-text-length (seconds-to-string (alist-get 'duration item)) 8 t)))
      (make-button (line-beginning-position) (line-end-position) 
		   'action action
		   'face fontface
		   'read-only t)
      (newline))
    (emdeez-ui-insert-playlist (cdr items) action)))

(defun emdeez-ui-play-this (button)
  "When clicking on a track (BUTTON) of the current playlist."
  (interactive)
  (let* ((startPos (button-start button))
	 (itemNumber (floor (/ (float startPos) 85))))
    (emdeez-player-play-pause (mapcar (lambda (x) (alist-get 'id x)) emdeez-player-playlist) itemNumber)
    )
  )

(defun emdeez-ui-make-shortcuts-bar (keylist)
  "Renders the shortcuts info bar at the bottom of the page"
  (let ((currentkey (car keylist)))
    (when currentkey ;; if non-nil (not the end of the loop)
      (let* ((cmd (replace-regexp-in-string (regexp-quote "emdeez-") "" (symbol-name (cdr currentkey)) nil 'literal))
	     (cmd (replace-regexp-in-string (regexp-quote "player-") "" cmd nil 'literal))
	     (cmd (replace-regexp-in-string (regexp-quote "ui-") "" cmd nil 'literal))
	     (cmd (replace-regexp-in-string (regexp-quote "make-frame") "" cmd nil 'literal)))
	(insert (propertize "[" 'face '(:height 0.7) 'display '(raise -1.0)))
	(insert (propertize (single-key-description (car currentkey)) 'face '(:height 0.7 :foreground "cyan") 'display '(raise -1.0)))
	(insert (propertize (concat "]" cmd " ") 'face '(:height 0.7) 'display '(raise -1.0))))
	(emdeez-ui-make-shortcuts-bar (cdr keylist)))))

(defun emdeez-ui-highlight-shortcut (str)
  "Highlight the shortcut key of the command STR."
  
  )

(defun emdeez-get-url-image (url)
  (let ((img (expand-file-name
	      (concat (md5 url) "." (file-name-extension url))
	      temporary-file-directory)))
    (unless (file-exists-p img)
      (url-copy-file url img))
    (let ((album-img (create-image img)))
      (setf (image-property album-img :scale) 0.1)
      album-img)))
;;      (insert-image album-img))))

(defun emdeez-ui-quit ()
  "Kill emacs music ui"
  (interactive)
  (when emdeez-ui-refresh-timer
    (cancel-timer emdeez-ui-refresh-timer))
  (delete-frame)
  )
(defun emdeez-ui-goto-playlist ()
  "Go back and forth between playlist and player."
  (interactive)
  (if (string= (buffer-name) "*emdeez*") ;; Skip the image
      (other-window 2)
    (other-window 1))) 

(defun emdeez-ui-seek()
  (interactive)
  (if (eq (line-number-at-pos) 3)
      (let* ((charPos (point)))
	(save-excursion 
	  (beginning-of-line)
	  (emdeez-player-seek (* 100 (/ (- (float charPos) (float (point))) emdeez-ui-progress-size)))))))

(defun emdeez-ui-motion ()
  "Save the emdeez cursor position."
  (if (eq (line-number-at-pos) 3)
      (let ((charPos (point))
	    (beg (save-excursion 
		   (beginning-of-line)
		   (point))))
	(setq emdeez-ui-cursor-pos (- charPos beg)))
      (setq emdeez-ui-cursor-pos nil)))

(provide 'emdeez-ui)

;;; emdeez-ui.el ends here

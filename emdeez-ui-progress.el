;;; emdeez-ui-progress.el --- A progress bar -*- lexical-binding: t -*-

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

;; Simple progress bar

;;; Code:

(defconst emdeez-ui-progress-char ?█ "Unit char used for the progress.")

(defcustom emdeez-ui-progress-size 90 "Size of the emdeez progress bar.")

(defun emdeez-ui-progress-update (currentTime minTime maxTime)
  "Update the emdeez progress bar with CURRENTTIME within [MINTIME; MAXTIME] limits."
  (unless (eq (- maxTime minTime) 0)
    (let ((progress (floor (* currentTime (/ (float emdeez-ui-progress-size) (- maxTime minTime))))))
      (insert
       (propertize (concat
		    (format-seconds "%.2m:%.2s" currentTime)
		    (make-string (floor (/ emdeez-ui-progress-size 2)) ? )
		    (format-seconds "%.2m:%.2s" (- currentTime maxTime)))
		   'display '(raise -0.2)))
      (newline)
      (insert (propertize (make-string progress emdeez-ui-progress-char)
			  'face `(:height 0.5 :foreground "#550000")))
      (insert (propertize (make-string (- emdeez-ui-progress-size progress) emdeez-ui-progress-char)
			  'face `(:height 0.5)))
      (newline)
      (when emdeez-ui-cursor-pos
        (let ((text-pos (if (< emdeez-ui-cursor-pos (- emdeez-ui-progress-size 5))
			    emdeez-ui-cursor-pos
			  (- emdeez-ui-progress-size 5))))
	  (insert (propertize
		   (concat
		    (make-string text-pos ? )
		    (format-seconds "%.2m:%.2s" (* (/ (float emdeez-ui-cursor-pos) emdeez-ui-progress-size) (- maxTime minTime))))
		   'face `(:height 0.6))))))))

(defun emdeez-ui-progress-motion (win prev-pos sym)
  "When the cursor moves, update the time on the seek bar."
  (if (eq (line-number-at-pos) 3)
      (let ((charPos (point))
	    (beg (save-excursion 
		   (beginning-of-line)
		   (point)))
	    (end (save-excursion 
		   (end-of-line)
		   (point)))
	    (currentTrack (nth (alist-get 'number emdeez-player-current-track) emdeez-player-playlist)))
	(message (format-seconds "%.2m:%.2s" (* (/ (- (float charPos) (float beg)) emdeez-ui-progress-size) (alist-get 'duration currentTrack)))))))

(provide 'emdeez-ui-progress)

;;; emdeez-ui-progress.el ends here

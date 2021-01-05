;;; emdeez-mode-line.el --- Emdeez in mode-line -*- lexical-binding: t -*-

;; Author: Thibaud Toullier
;; Maintainer: Thibaud Toullier
;; Version: 0.9.0-alpha
;; Package-Requires: (emdeez)

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

;; This is an extra module to Emdeez.
;; It shows the current played track to the modeline.
;;
;; Default format is: Artist - Title SpentTime/TotalTime|
;; where SpentTime and TotalTime have the format : `%.2m:%.2s' (mm:ss)

;;; Code:
(require 'emdeez)

(defcustom emdeez-mode-line-refresh
  nil
  "Either to automatically refresh the modeline.
⚠ t value can cause latencies (nil by default)

See also: `emdeez-mode-line-refresh-rate'")
(defcustom
  emdeez-mode-line-refresh-rate
  20
  "Defines the refresh rate of the modeline.
⚠ works only if `emdeez-mode-line-refresh' is set to t.

See also: `emdeez-mode-line-refresh'")

(defun emdeez-mode-line-info ()
  "Return the current track played information to be displayed in the modeline.

📌 TODO: add more customization such as display order, font size etc."
  (when (and (oref emdeez-server user) emdeez-player-current-track)
    (if emdeez-mode-line-refresh
	(when (> (float-time (time-subtract (float-time) emdeez-mode-line-refresh)) emdeez-mode-line-refresh-rate)
	  (emdeez-refresh)
	  (setq emdeez-mode-line-refresh (float-time)))
      (setq emdeez-mode-line-refresh (float-time)))
    (let ((currentTrack (nth (alist-get 'number emdeez-player-current-track) emdeez-player-playlist)))
      (concat
       (propertize
	(alist-get 'name (alist-get 'artist currentTrack))
	'face 'bold)
       " - "
       (propertize
	(alist-get 'title currentTrack)
	'face 'mode-line)
       " "
       (let ((currentTime 
	      (if (= emdeez-player-status 0)
		  (alist-get 'time-paused emdeez-player-current-track)
		(float-time (time-subtract (float-time) (alist-get 'time-started emdeez-player-current-track)))))
	     (duration (alist-get 'duration currentTrack)))
	 (if (<= currentTime duration)
	     (propertize
	      (concat
	       (format-seconds "%.2m:%.2s" currentTime)
	       "/"
	       (format-seconds "%.2m:%.2s" duration))
	      'face 'shadow)
	   (propertize "--:--/--:--" 'face 'shadow)
	   (emdeez-refresh)))
       (propertize
	"|"
	'face 'shadow)))))

(add-to-list 'global-mode-string '(:eval (emdeez-mode-line-info)))

(provide 'emdeez-mode-line)

;;; emdeez-mode-line.el ends here

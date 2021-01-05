;;; emdeez-player.el --- Actions for emdeez-player -*- lexical-binding: t -*-

;; Author: Thibaud Toullier
;; Maintainer: Thibaud Toullier
;; Version: 0.9.0-alpha
;; Package-Requires:
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

;; This code defines the basic actions of the player
;; such as play/pause/previous/forward etc.

;;; Code:

(defvar emdeez-player-playlist nil "Current playlist.")

(defvar emdeez-player-status 0 "Status of emdeez-player.  0 - pause.  1 - play.")

(defvar emdeez-player-current-track
  '((number . 0)
   (time-started . 0)
   (time-paused . 0))
  "Current track # of playlist and when it was started.")

;;;; Functions

(defun emdeez-player--parse-flow (events)
  "Parse the JSON flow based on EVENTS response."
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'alist)
	(json-key-type 'symbol)
	(json-array-type 'list))
    (let* ((result (cdar (json-read))))
      (setq emdeez-player-playlist result)
      (emdeez-player-play-pause (mapcar (lambda (x) (alist-get 'id x)) result))
      )))

;;;; Commands

;;;###autoload
(defun emdeez-player-play-pause (&optional list itemNumber)
  "Resume or pause current track playing.  If LIST is provided, set it as current tracklist.  If ITEMNUMBER is provided, start from this item."
  (interactive)
  (let ((command-to-send))
    (if list
	(progn
	  (setq command-to-send (concat "$tracks:" (json-encode-list list)))
	  (emdeez-player-update 0)
	  (when itemNumber
	    (setq command-to-send (concat "$item:" (number-to-string itemNumber)))))
      (if (eq emdeez-player-status 0)
	  (progn
	    (setq command-to-send "$play:null")
	    (setq emdeez-player-status 1)
	    (setf (alist-get 'time-started emdeez-player-current-track) (float-time (time-subtract (float-time) (alist-get 'time-paused emdeez-player-current-track)))))
	(setq command-to-send "$pause:null")
	(setq emdeez-player-status 0)
	(setf (alist-get 'time-paused emdeez-player-current-track) (float-time (time-subtract (float-time) (alist-get 'time-started emdeez-player-current-track))))))
    (emdeez-server-query emdeez-server command-to-send nil)))

;;;###autoload
(defun emdeez-player-next ()
  "Play next song of current playlist."
  (interactive)
  (emdeez-server-query emdeez-server "$next:null" nil)
  (let ((n (alist-get 'number emdeez-player-current-track)))
    (emdeez-player-update (+ n 1))))

;;;###autoload
(defun emdeez-player-previous ()
  "Play previous song of current playlist."
  (interactive)
  (emdeez-server-query emdeez-server "$prev:null" nil)
  (let ((n (alist-get 'number emdeez-player-current-track)))
    (emdeez-player-update (- n 1))))

;;;###autoload
(defun emdeez-player-update (iTrack &optional iTime)
  "Update the current track number ITRACK in playlist at time ITIME.  If ITIME is nil, the time is set to current time."
  (setf (alist-get 'number emdeez-player-current-track) iTrack)
  (if iTime
      (setf (alist-get 'time-started emdeez-player-current-track) iTime)
    (setf (alist-get 'time-started emdeez-player-current-track) (float-time))))

;;;###autoload
(defun emdeez-player-seek (pos)
  "Move current song cursor to POS in percent."
  (interactive)
  (emdeez-server-query emdeez-server (concat "$seek:" (number-to-string pos)) nil)
  (emdeez-refresh))

;;;###autoload
(defun emdeez-player-flow ()
  "Load user's flow as current playlist."
  (interactive)
  (let ((emdeez-user (oref emdeez-server user)))
    (if emdeez-user
	(url-retrieve (concat (alist-get 'tracklist emdeez-user) "?limit=50") 'emdeez-player--parse-flow)
      (message "User seems to be undefined! Cannot load flow."))
    )
  )

(provide 'emdeez-player)

;;; emdeez-player.el ends here

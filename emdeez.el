;;; emdeez.el --- Provides a player for Deezer interaction -*- lexical-binding: t -*-

;; Author: Thibaud Toullier
;; Maintainer: Thibaud Toullier
;; Version: 0.9.0-alpha
;; Package-Requires: ((eieio) (json) (websocket "1.12") (uuidgen "1.2") (simple-httpd "1.5.1"))
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

;; This program enables the control of Deezer through Emacs.
;; You can visit the homepage at URL `https://github.com/KirmTwinty/emdeez'
;; for more details.

;;;; Installation:

;; Pick-up the installation method that fits your needs.

;;;;; MELPA

;;;;; Use-package

;;;;; Manual - Use-package and Quelpa

;; You can use `use-package' and `quelpa' for installing Emdeez:

;; (use-package emdeez
;;   :quelpa ((emdeez :fetcher github :repo "KirmTwinty/emdeez"))
;;   :init
;;     (use-package websocket)
;;     (use-package uuidgen)
;;     (use-package simple-httpd)
;;     (use-package all-the-icons)
;;   :requires (websocket uuidgen simple-httpd all-the-icons)
;;   :bind (("C-c m" . emdeez))
;;   :after
;;     (require 'emdeez-mode-line) ;; optionally for modeline information
;;     (add-to-list 'ibuffer-never-show-predicates "^\\*emdeez") ;; optionally hide buffers from iBuffer list
;; )
;;;;; Manual

;; You can also install it entirely by hand.

;; 1. Install the required packages: `websocket' (version 1.12), `uuidgen' (version 1.2), `simple-httpd' (version 1.5.1)
;; 2. Clone Emdeez repository https://github.com/KirmTwinty/emdeez
;; 3. Add the downloaded folder to your load path and put this in your init file:

;; ;; Add Emdeez folder to your load path
;; (add-to-list 'load-path "<emdeez-folder>")
;; (require 'emdeez)
;; (require 'emdeez-mode-line)            ;; optionaly for modeline information
;; (global-set-key (kbd "C-c m") 'emdeez) ;; optionaly to assign a key to Emdeez

;;; Usage

;; Launch `emdeez' command \\[emdeez]
;; A web browsing window will open asking to authenticate yourself to Deezer.
;; Once the authentication is made, re-run `emdeez' command to see the player.

;; Note: the websocket connection between Emacs and the server can have some various delay.
;;       As long as "Waiting for the socket to be opened" message is displayed on the HTML page,
;;       the player won't open.
;;       Everything is setup once the message "You can now control the player within Emacs!"
;;       is displayed on the HTML page.
;;
;; Thanks to https://alphapapa.github.io/emacs-package-dev-handbook/ for helping me developing this package.

;;; Code:

;;;; Requirements
(require 'websocket)
(require 'emdeez-server)
(require 'emdeez-ui)
(require 'emdeez-ui-search)

;;;; Customization

(defgroup emdeez
  nil
  "Settings for `emdeez'."
  :link '(url-link "https://github.com/KirmTwinty/emdeez"))

(defcustom emdeez-path
  nil
  "Absolute full path of emdeez installation.")

;;;; Variables
(defvar emdeez-server
  nil
  "Emdeez server instance.
See also: `emdeez-server-cl'")

(defvar emdeez-url-opened
  nil
  "If the Emdeez url browse was already called.")

;;;; Keymaps

(defvar emdeez-mode-map
  (let ((map (make-sparse-keymap))
	(maps (list
	       ;; UI keys
	       "C-q" #'emdeez-ui-quit
	       ;; "h" #'emdeez-ui-help
	       ;; Player keys
	       "RET" #'emdeez-ui-seek
	       "g" #'emdeez-refresh
	       "o" #'emdeez-ui-goto-playlist
	       "s" #'emdeez-ui-search-make-frame
	       "f" #'emdeez-player-flow
	       "n" #'emdeez-player-next
	       "p" #'emdeez-player-previous
	       "SPC" #'emdeez-player-play-pause
	       )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map)
  "Keymap for emdeez buffer.")

(defvar emdeez-search-mode-map
  (let ((map (make-sparse-keymap))
	(maps (list
	       ;; UI keys
	       "C-q" #'emdeez-ui-search-quit
	       ;;"C-h" #'emdeez-ui-help
	       ;; Search keys
	       "<C-return>" #'emdeez-ui-search-query
	       )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map)
  "Keymap for emdeez-search buffer.")

;;;; Functions

(defun emdeez--refresh-callback (response)
  "Callback executed to refresh data according to RESPONSE.
See also: `emdeez-server--process-message'."
  (emdeez-player-update (alist-get 'currentIndex response) (float-time (time-subtract (float-time) (alist-get 'position response)))))

;;;; Functions utilities
(defun emdeez-utils--path-join (path &rest str)
  "Concat strings in STR to PATH recursively.

Example
-------

```elisp
\(emdeez-utils--path-join \"/home\" \"user\" \"some\" \"other\" \"file.el\")
;; returns \"/home/user/some/other/file.el\" on Linux
```"
  (if str
      (apply 'emdeez-utils--path-join (cons (concat (file-name-as-directory path) (car str)) (cdr str)))
    path))

;;;; Commands

;;;###autoload
(defun emdeez-refresh ()
  "Ask the server for refresh update."
  (interactive)
  (emdeez-server-query emdeez-server "$refresh:null" nil))

;;;###autoload
(defun emdeez()
  "`emdeez' enables the control of Deezer through Emacs.
- If the server is not running, start it.
- Otherwise, show emdeez-ui.

Usage
-----

- Launch `emdeez' command \\[emdeez]
- A web browsing window will open asking to authenticate yourself to Deezer.
- Once the authentication is made, re-run `emdeez' command to see the player.

Note
----

The websocket connection between Emacs and the server can have some various
delay.
As long as \"Waiting for the socket to be opened\" message is displayed
on the HTML page, the player won't open.
Everything is setup once the message
\"You can now control the player within Emacs!\"
is displayed on the HTML page.

You can visit the homepage at URL `https://github.com/KirmTwinty/emdeez'
for more details.

Emdeez UI Keybindings
---------------------
\\{emdeez-mode-map}

Emdeez UI-search Keybindings
----------------------------
\\{emdeez-search-mode-map}"
  (interactive)
  
  ;; start the server
  (unless emdeez-server
    (setq emdeez-server (make-instance 'emdeez-server-cl))
    (emdeez-server-start emdeez-server)
    ;; Let the server start
    (sleep-for 1))

  ;; check if the user informations were fetched
  (unless (oref emdeez-server user)
    (emdeez-server--fetch-user))
  ;; check if server has the html socket connected
  (if (emdeez-server-client-ready-p emdeez-server)
      (emdeez-ui-make-frame)
    (if emdeez-url-opened ;; We do not want to spam users with browser windows
	(message (concat "If the authentication went well, please wait a bit. Otherwise, go back to " (oref emdeez-server address) ":" (oref emdeez-server http-port)))
	;; We open a window for the connection
      (message "Please authenticate to Deezer and call Emdeez again.")
      (browse-url (concat "http://" (oref emdeez-server address) ":" (oref emdeez-server http-port)))
      (setq emdeez-url-opened t))))

;;;; Footer

(provide 'emdeez)

;;; emdeez.el ends here

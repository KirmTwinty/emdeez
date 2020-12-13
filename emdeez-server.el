;;; emdeez-server.el --- Provides a websocket server for communicating between the HTML page and Emacs. -*- lexical-binding: t -*-

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

;; This code is part of Emdeez.
;; It provides a server websocket to communicate
;; between Emacs user and the Deezer JS SDK and API.
;;
;; It also creates a HTTPD server for serving the HTML
;; file that contains Deezer's JS SDK.
;;
;; By default, the HTTP server is at URL `http://127.0.0.1:8000'
;; By default, the WS server is at URL `ws://127.0.0.1:8000'
;;
;; For more information, visite URL `https://github.com/KirmTwinty/emdeez'

;;; Code:

(require 'eieio)
(require 'uuidgen)
(require 'simple-httpd)
(require 'json)
(require 'websocket)

(defgroup emdeez-server nil "Emdeez server customization")

(defclass emdeez-server-cl()
  ((address :initarg :address
	    :initform "127.0.0.1"
	    :type string
	    :custom string
	    :group emdeez-server
	    :documentation
	    "Emdeez websocket server address.")
   (port :initarg :port
	    :initform "8080"
	    :type string
	    :custom string
	    :group emdeez-server
	    :documentation
	    "Emdeez websocket server port.")
  (clients :initform nil
	   :allocation :instance
	   :documentation
	   "Emdeez websocket server connected clients list.")
  (server :initform nil
	   :allocation :instance
	   :documentation
	   "Emdeez websocket server object holder.")
  (running :initform nil
	   :allocation :instance
	   :documentation
	   "Emdeez websocket server status.")
  (ready :initform nil
	 :allocation :instance
	 :documentation
	 "Whether the HTML client is connected.")
  (http-port :initarg :port
	     :initform "8000"
	     :type string
	     :custom string
	     :group emdeez-server
	     :documentation
	     "HTTP server port.")
  (user :initform nil
	:allocation :instance
	:documentation
	"User id")
  (token :initform nil
	 :allocation :instance
	 :documentation
	 "Current Deezer id token")
  (last-message :initform nil
		:allocation :instance
		:documentation
		"Last message received by the client from the server.
📌 TODO: change this to a FIFO queue."))
  :documentation
  "A websocket server class to make the communication between the HTML page and Emacs through the Emdeez client class.")


(cl-defmethod emdeez-server--add-client ((obj emdeez-server-cl) client-ws)
  "Add the CLIENT-WS to the EM-SERVER.
Function called when a client is connects to the server.
A ping/pong exchange is then performed where the server sends a unique id
to the HTML client which gives back the user informations.

See also: `emdeez-server--process-message', `emdeez-server--fetch-user', `emdeez-server--set-user-callback'"
  (let ((id (uuidgen-4))
	(clients (oref obj clients)))
    (emdeez-server--send obj client-ws `(("command" . "identification") ("value" . ,id)))
    (oset obj clients (list (list client-ws id))) ;; only one client
    ;; Code for multi-client support
    ;; (useless since only one instance of Deezer can be set)
    ;; (if clients
    ;; 	(oset obj clients (add-to-list (oref obj clients) (list client-ws id)))
    ;;   (oset obj clients (list (list client-ws id))))
    ))

(defun emdeez-server--parse-response (events callback)
  "Parse the response in EVENTS returned by `url-retrieve` then execute CALLBACK."
  (goto-char url-http-end-of-headers)
  (let ((json-object-type 'alist)
	(json-key-type 'symbol)
	(json-array-type 'list))
    (let* ((result (cdar (json-read))))
      (apply callback (list result)))))

(cl-defmethod emdeez-server-query ((obj emdeez-server-cl) query callback &optional wait-for-response)
  "Perform a query QUERY to Deezer through the emdeez-server.
A simple query can be 'Amon Amarth'.
A more complex one can be 'artist:\"Amon Amarth\"track:\"Twilight Of The Thunder God\".'
Execute CALLBACK when the response is obtained.
If WAIT-FOR-RESPONSE is non-nil then url-retrieve is made syncronously

For more information: URL `https://developers.deezer.com/api'"
  (if (string-match-p (regexp-quote "$") query)   ;; Check if it is a command for the player
      (save-match-data
	(string-match "\\$\\([a-z]*\\)\\:\\(.*\\)" query)
	(let* ((command (match-string 1 query))
	       (value (match-string 2 query)))
	  ;;(message (concat "Sending " command " with value " value " to JS player."))
	  (emdeez-server--send obj (caar (oref obj clients)) `(("command" . ,command) ("value" . ,value)))))
    ;; otherwise query Deezer web API directly
    (if wait-for-response
	(emdeez-server--parse-response (url-retrieve-synchronously
				(concat "https://api.deezer.com/search"
					"?acces_token=" (oref obj token)
					"&q=" query)))
      (url-retrieve
	 (concat "https://api.deezer.com/search"
		 "?acces_token=" (oref obj token)
		 "&q=" query)
	 'emdeez-server--parse-response (list callback)))
    )
  ) ;; otherwise transmit the message to the deezer API
  
;; (let ((emdeez-ws (oref obj clients))
;; 	(emdeez-response nil))
;;     (emdeez-server--send emdeez-ws)
;;     (when wait-for-response
;;       (while (not (oref obj last-message)) ;; as long as the response hasn't been received !!! ;; TODO break in case
;; 	(sleep-for 0.1))
;;       (let ((emdeez-msg (oref obj last-message)))
;; 	(if (string= "" emdeez-msg)
;; 	    (message "No playlist. Perform a search or select your flow first.")
;; 	  (let ((json-object-type 'alist)
;; 		(json-key-type 'symbol)
;; 		(json-array-type 'list))
;; 	    (setq emdeez-response (json-read-from-string emdeez-msg))))))
;;     ;; last message was processed, reset it
;;     (oset obj last-message nil)
;;     emdeez-response)) ;; return the response

(cl-defmethod emdeez-server--send ((obj emdeez-server-cl) client-ws data)
  "Send DATA to the CLIENT-WS.
Please note that all data are `json-encode' before being sent."
  (websocket-send-text client-ws (json-encode data)))

(defun emdeez-server--set-user-callback (response)
  "Set the user data from RESPONSE."
  (goto-char url-http-end-of-headers)
  (let ((val (cdr (json-read))))
    (oset emdeez-server user val)
    (message val)
    )
  )

(defun emdeez-server--fetch-user ()
  "Retrieve the user informations."
  (url-retrieve
   (concat "https://api.deezer.com/user/me"
	   "?access_token=" (oref emdeez-server token))
   'emdeez-server--set-user-callback))

(cl-defmethod emdeez-server--process-message ((obj emdeez-server-cl) emdeez-msg)
  "Process incoming messages EMDEEZ-MSG from the HTML websocket (binded to `:on-message')"
  (oset obj last-message emdeez-msg) ;; store last-message

  (let* ((json-data (json-read-from-string emdeez-msg))
	 (cmd (alist-get 'command json-data))
	 (value (alist-get 'value json-data)))
    ;;(message (concat "Received: " emdeez-msg))
    (cond ((string= "identification" cmd)
	   (progn
	     ;; Get all user information
	     (emdeez-server--fetch-user)
	     (oset obj token (alist-get 'token value))
	     (oset obj ready t)))
	  ((string= "refresh" cmd)
	   (emdeez--refresh-callback value)))))

(cl-defmethod emdeez-server-start ((obj emdeez-server-cl))
  "Start the websocket server as well as the HTTP one for serving the HTML file."
  (oset obj server (websocket-server
		    (oref obj port)
		    :host (oref obj address)
		    :on-open (lambda (_websocket)
			       (message "A new backend is connected")
			       (emdeez-server--add-client obj _websocket))
		    :on-message (lambda (_websocket frame)
				  (emdeez-server--process-message obj (websocket-frame-text frame)))
		    :on-close (lambda (_websocket)
				(oset obj running nil)
				(oset obj clients nil))
		    :on-error (lambda (_websocket symbol msg)
				(message "Emdeez-server: An error occured during" symbol ": " msg)
				;;(oset obj running nil)
				)))
  (setq httpd-host (oref obj address)
	httpd-port (oref obj http-port)
	httpd-root (emdeez-utils--path-join emdeez-path "html"))
  (defun httpd/channel (proc path &rest args)
    (httpd-discard-buffer)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents "/home/ttoullie/Documents/Misc/git/emdeez-git/html/channel.html")
      (httpd-send-header proc (httpd-get-mime (file-name-extension "/home/ttoullie/Documents/Misc/git/emdeez-git/html/channel.html")) 200
			 :Pragma "public"
			 :Cache-Control "maxage=31536000"
			 :Expires "Tue, 16 Nov 2021 07:28:00 GMT")))
  (httpd-start) ;; only one instance / Emacs instance
)

(cl-defmethod emdeez-server-client-ready-p ((obj emdeez-server-cl))
  "Returns t if the client is connected, nil otherwise."
  (oref obj ready))

(provide 'emdeez-server)
;;; emdeez-server ends here

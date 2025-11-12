(ql:quickload 'websocket-driver-client)

(defpackage :forrest-gump
  (:use :websocket-driver))
(in-package :forrest-gump)

(defun prompt-user (prompt)
  (format *query-io* "~A" prompt)
  (force-output *query-io*)
  (read-line *query-io*))
;; Setting up global variables
;;(defvar *bot-token* (concatenate 'string "Bot " (prompt-user "Bot token: ")))
(defvar *wss-base* "wss://gateway.discord.gg/?v=10&encoding=json")
(defvar *client* (make-client *wss-base*))
;; Setting up WSS actions
(on :open *client*
        (lambda ()
          (format t "Connected to WSS")))
(on :message *client*
        (lambda (msg)
          (format t "~S~%" msg)))
(on :close *client*
        (lambda (&key code reason)
          (format t "Closed because '~A' (Code=~A)~%" reason code)))

(start-connection *client*)

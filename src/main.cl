;;;; file: /Users/urmoter/Desktop/Forrest-Gump-Discord-Bot/src/main.cl

(ql:quickload '(websocket-driver-client cl-json bordeaux-threads drakma))

(defpackage :main
  (:use :cl)
  (:local-nicknames
    (:bt :bordeaux-threads)
    (:http :drakma)))
(in-package :main)

(defun prompt-user (prompt)
  (format *query-io* "~A" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; ----------------------------------------------------------------------
;; Globals
;; ----------------------------------------------------------------------

(defvar *wss-base* "wss://gateway.discord.gg/?v=10&encoding=json")
(defvar *rest-base* "https://discord.com/api/v10")
(defvar *client*    (wsd:make-client *wss-base*))

(defvar *hb-int*)
(defvar *previous-event* nil)
(defvar *hb-thread* nil)
(defvar *hb-run* nil)
(defvar *first-ack-received* nil)
(defvar *token* nil)
(defvar *self-user-id* nil)

;; ----------------------------------------------------------------------
;; Heartbeat
;; ----------------------------------------------------------------------

(defun send-heartbeat ()
  (let* ((seq (if (integerp *previous-event*)
                  *previous-event*
                  nil))
         (payload
           (json:encode-json-to-string
             `((:OP . 1)
               (:D  . ,seq)))))
    (format t "Heartbeat with seq: ~S~%Payload: ~A~%" seq payload)
    (wsd:send *client* payload)))

(defun start-heartbeat ()
  (when *hb-int*
    (unless (and *hb-thread* (bt:thread-alive-p *hb-thread*))
      (setf *hb-run* t)
      (setf *hb-thread*
            (bt:make-thread
             (lambda ()
               (loop while *hb-run*
                     do (send-heartbeat)
                        (sleep (/ *hb-int* 1000.0)))))))))

(defun stop-heartbeat ()
  (setf *hb-run* nil)
  (when (and *hb-thread* (bt:thread-alive-p *hb-thread*))
    (bt:join-thread *hb-thread*)))

;; ----------------------------------------------------------------------
;; Identification
;; ----------------------------------------------------------------------

(defun identify ()
  (wsd:send *client*
            (json:encode-json-to-string
             `((:OP . 2)
               (:D  . ((:TOKEN . ,(concatenate 'string "Bot " *token*))
                       (:INTENTS . 34305)
                       (:PROPERTIES . ((:OS      . "SBCL")
                                       (:BROWSER . "SBCL")
                                       (:DEVICE  . "SBCL")))))))))

;; ----------------------------------------------------------------------
;; REST helpers
;; ----------------------------------------------------------------------

(defun http-post (endpoint body)
  "BODY must be a JSON string."
  (drakma:http-request (concatenate 'string *rest-base* endpoint)
                       :method :post
                       :content body
                       :additional-headers
                       `(("User-Agent" .
                          "DiscordBot (https://github.com/urmoter/Forrest-Gump-Discord-Bot, SBCL-1.0)")
                         ("Authorization" .
                          ,(concatenate 'string "Bot " *token*))
                         ("Content-Type" . "application/json"))))

(defun send-message (channel-id content reply &optional reply-message-id)
  (let ((body `(("content" . ,content)
                ("tts"     . nil))))
    (when reply
      (push `("message_reference" . (("message_id" . ,reply-message-id)))
            body))
    (http-post (concatenate 'string "/channels/" channel-id "/messages")
               (json:encode-json-to-string body))
    t))

;; ----------------------------------------------------------------------
;; WebSocket event handlers
;; ----------------------------------------------------------------------

(wsd:on :open *client*
        (lambda ()
          (format t "Connected to WSS~%")))

(wsd:on :message *client*
        (lambda (msg)
          (block handle-message
            (let ((data (json:decode-json-from-string msg)))
              ;; Track sequence
              (let ((s-pair (assoc :S data)))
                (when s-pair
                  (setf *previous-event* (cdr s-pair))))
              ;; Handle opcodes
              (case (cdr (assoc :OP data))
                (10
                 (setf *hb-int*
                       (cdr (assoc :HEARTBEAT--INTERVAL
                                   (cdr (assoc :D data)))))
                 (format t "Heartbeat interval: ~A ms~%~22T~,2F s~%"
                         *hb-int* (/ *hb-int* 1000.0))
                 (start-heartbeat))

                (11
                 (format t "Heartbeat ACK~%")
                 (unless *first-ack-received*
                   (setf *first-ack-received* t)
                   (identify)))

                (0
                 (let* ((etype (cdr (assoc :T data)))
                        (edata (cdr (assoc :D data))))
                   (cond
                     ;; READY
                     ((string= etype "READY")
                      (setf *self-user-id*
                            (cdr (assoc :ID (cdr (assoc :USER edata)))))
                      (format t "READY, UID: ~A~%" *self-user-id*))

                     ;; MESSAGE_CREATE
                     ((string= etype "MESSAGE_CREATE")
                      (let* ((author     (cdr (assoc :AUTHOR edata)))
                             (author-id  (cdr (assoc :ID author)))
                             (username   (cdr (assoc :USERNAME author)))
                             (channel-id (cdr (assoc :CHANNEL--ID edata)))
                             (content    (cdr (assoc :CONTENT edata)))
                             (message-id (cdr (assoc :ID edata)))
                             (mentions   (cdr (assoc :MENTIONS edata)))
                             (mentions-everyone (cdr (assoc :MENTION--EVERYONE edata))))

                        ;; Debug info
                        ; (format t "Message ID: ~A~%" message-id)
                        ; (format t "Message Content: ~S~%" content)
                        ; (format t "Author: ~A <@~A>~%" username author-id)
                        ; (format t "In Channel: ~A~%" channel-id)
                        ; (dolist (m mentions)
                        ;   (format t "~&Mention: ~A (~A)~%"
                        ;           (cdr (assoc :USERNAME m))
                        ;           (cdr (assoc :ID m))))
                        ; (format t "~%")

                        ;; EARLY BAIL: ignore own messages completely
                        (when (and *self-user-id* author-id
                                   (string= author-id *self-user-id*))
                          (format t "Ignoring my own message.~%")
                          (return-from handle-message))

                        ;; If bot is mentioned, reply "Yes, Jennay?"
                        (let ((mentioned-self-p
                                (some (lambda (m)
                                        (let ((id (cdr (assoc :ID m))))
                                          (and id
                                               *self-user-id*
                                               (string= id *self-user-id*))))
                                      mentions)))
                          (when mentioned-self-p
                            (send-message channel-id
                                          "Yes, Jennay?"
                                          t
                                          message-id)))

                        ;; If message content contains 'run' (case-insensitive)
                        (when (and content
                                   (search "run" content
                                           :test #'char-equal))
                          (send-message channel-id
                                        "https://tenor.com/view/run-forest-run-forest-gump-tom-hanks-gif-5306812"
                                        nil)
                          (send-message channel-id
                                        "RUN FORREST, RUN!"
                                        nil)
                        )
                        ;; If message content contains 'chocolate' (case-insensitive)
                        (when (and content
                                   (search "chocolate" content
                                           :test #'char-equal))
                          (send-message channel-id
                                        "https://tenor.com/view/rolfismus-rolfen-rolf-rolfreunde-rolf-chocolates-gif-25382821"
                                        nil)
                          (send-message channel-id
                                        "Life is like a box of chocolates!"
                                        nil)
                        )
                        ;; If message contains @everyone
                        (when mentions-everyone
                          (send-message channel-id
                                        "https://tenor.com/view/bored-miserable-waste-time-forrest-gump-ping-pong-gif-9282097"
                                        nil)
                        )
                    )))))

                (t
                 (format t "~A~%" msg)))))))

(wsd:on :close *client*
        (lambda (&key code reason)
          (format t "Closed because '~A' (Code=~A)~%" reason code)
          (stop-heartbeat)
          (setf *first-ack-received* nil)))

;; ----------------------------------------------------------------------
;; Entry point
;; ----------------------------------------------------------------------

(setf *token* (prompt-user "Token: "))
(wsd:start-connection *client*)

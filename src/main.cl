(ql:quickload '(websocket-driver-client jsown bordeaux-threads))

(defpackage :forrest-gump
    (:use :cl)
    (:import-from :websocket-driver
        :make-client :on :start-connection :ready-state :send :close-connection)
    (:nicknames :dd)
)
(in-package :forrest-gump)

;; ----------------------------------------------------------------------
;; Globals
;; ----------------------------------------------------------------------
(defvar *ws* nil)             ; websocket handle
(defvar *seq* nil)            ; last sequence number from gateway
(defvar *hb-thread* nil)      ; heartbeat thread
(defvar *hb-interval* nil)    ; ms between heartbeats
(defvar *gateway-thread* nil) ; background runner
(defvar *hb-run* nil)         ; run flag for heartbeat loop checks


;; ----------------------------------------------------------------------
;; Utils
;; ----------------------------------------------------------------------
(defun ws-send-json (obj)
    (send *ws* (jsown:to-json obj))
)

(defun alivep (thr)
    (and thr (bt:thread-alive-p thr))
)

(defun kill-thread (thr)
    (when (alivep thr)
        (bt:destroy-thread thr))
)

;; ----------------------------------------------------------------------
;; Heartbeat
;; ----------------------------------------------------------------------
(defun start-heartbeat (interval-ms)
    (when (and *hb-thread* (bt:thread-alive-p *hb-thread*))
        (setf *hb-run* nil)
        (bt:join-thread *hb-thread*))
    (setf *hb-interval* interval-ms
          *hb-run* t
          *hb-thread*
          (bt:make-thread
           (lambda ()
               (loop while *hb-run*
                   do
                       (sleep (/ interval-ms 1000.0))
                       (when *hb-run*
                           (handler-case
                               (ws-send-json
                                   (jsown:new-js
                                       ("op" 1)      ; HEARTBEAT
                                       ("d" *seq*)   ; last seq or null
                                   )
                               )
                               (t (e)
                                   (format t "~&Heartbeat error: ~A~%" e)
                                   (return)
                               )
                           )
                       )
               )
           )
          )
    )
)

;; presence helper (optional)
(defun go-invisible ()
    (ws-send-json
        (jsown:new-js
            ("op" 3) ; PRESENCE UPDATE
            ("d" (jsown:new-js
                    ("since" :null)
                    ("activities" '())
                    ("status" "invisible")
                    ("afk" t)
                 ))
        )
    )
)

;; ----------------------------------------------------------------------
;; Payload handling
;; ----------------------------------------------------------------------
(defun handle-payload (text)
    (let* ((j (jsown:parse text))
           (op (jsown:val j "op"))
           (type (jsown:val j "t"))
           (s (jsown:val j "s"))
           (d (jsown:val j "d")))
        (setf *seq* s)
        (case op
            (10 ; HELLO
                (let ((interval (jsown:val d "heartbeat_interval")))
                    (format t "~&HELLO; heartbeat every ~A ms~%" interval)
                    (start-heartbeat interval)
                )
            )
            (11 ; HEARTBEAT_ACK
                (format t "~&HB ACK~%")
            )
            (0  ; DISPATCH
                (case (string type)
                    ("MESSAGE_CREATE" ; User sends a message
                        (let* ((member (jsown:val d "member"))
                               (guild-id (jsown:val d "guild_id"))
                               (mentions (jsown:val d "mentions"))
                              )
                            (format t "~&Who? ~A~%Where? ~A~%Mentions? ~A~%" member guild-id mentions)
                        )
                    )

                    (t
                        (format t "~&EVENT ~A~%" type)
                    )
                )
            )
            (t
                (format t "~&OP ~A: ~A~%" op d)
            )
        )
    )
)

;; ----------------------------------------------------------------------
;; Connection wiring (non-blocking)
;; ----------------------------------------------------------------------
(defun connect ()
    (setf *ws* (make-client "wss://gateway.discord.gg/?v=10&encoding=json"))

    (on :open *ws*
        (lambda ()
            (format t "CONNECTED.~%")
        )
    )

    (on :message *ws*
        (lambda (msg)
            (typecase msg
                (string (handle-payload msg))
                (t (format t "~&[NON-STRING MESSAGE] ~S~%" msg))
            )
        )
    )

    (on :error *ws*
        (lambda (e)
            (format t "~&ERROR: ~A~%" e)
        )
    )

    (on :close *ws*
        (lambda (&key code reason)
            (format t "~&Closed (~A): ~A~%" code reason)
        )
    )

    (start-connection *ws*)
    :started
)

;; replace start-gateway with this
(defun start-gateway ()
    (when (and *gateway-thread* (bt:thread-alive-p *gateway-thread*))
        (bt:destroy-thread *gateway-thread*))
    (setf *gateway-thread*
          (bt:make-thread
           (lambda ()
               ;; redirect all gateway prints so REPL prompt stays free
               (let ((*standard-output* *trace-output*)
                     (*error-output* *trace-output*))
                   (connect)))
           :name "discord-gateway"))
    :started)


(defun stop-gateway (&key (make-invisible t) (wait-ms 2000))
    ;; 1) optional: flip presence so clients drop the green dot fast
    (when (and make-invisible *ws* (eq (ready-state *ws*) :open))
        (ignore-errors (go-invisible))
        (sleep 0.05))  ;; tiny yield so it can flush

    ;; 2) stop heartbeats first, then close socket
    (when (and *hb-thread* (bt:thread-alive-p *hb-thread*))
        (setf *hb-run* nil)
        (bt:join-thread *hb-thread*)
        (setf *hb-thread* nil))

    (when *ws*
        (ignore-errors (close-connection *ws*))
        ;; 3) wait for real close (ready-state to leave :open/:connecting)
        (let ((deadline (+ (get-internal-real-time)
                           (* wait-ms (/ internal-time-units-per-second 1000)))))
            (loop while (member (ready-state *ws*) '(:connecting :open))
                  while (< (get-internal-real-time) deadline)
                  do (sleep 0.05))))
    ;; 4) clean up the gateway runner thread
    (when (and *gateway-thread* (bt:thread-alive-p *gateway-thread*))
        (bt:join-thread *gateway-thread*)
        (setf *gateway-thread* nil))

    ;; 5) clear globals
    (setf *ws* nil
          *seq* nil
          *hb-interval* nil)
    :stopped
)

;; ----------------------------------------------------------------------
;; Identify (call from REPL once connected)
;; ----------------------------------------------------------------------
(defun identify (token &key (intents 513) (os "linux") (browser "sbcl") (device "sbcl"))
    "Call after HELLO (safe to call soon after start). TOKEN must include the 'Bot ' prefix."
    (declare (type string token))
    (ws-send-json
        (jsown:new-js
            ("op" 2)
            ("d" (jsown:new-js
                    ("token" token)
                    ("intents" intents)
                    ("properties" (jsown:new-js
                                    ("os" os)
                                    ("browser" browser)
                                    ("device" device)
                                 ))
                 ))
        )
    )
)

;; ----------------------------------------------------------------------
;; Optional: manual heartbeat tick (debug)
;; ----------------------------------------------------------------------
(defun ping ()
    (ws-send-json (jsown:new-js ("op" 1) ("d" *seq*)))
    :sent
)

(in-package :forrest-gump)
(start-gateway)
(sleep 1)
(identify "")
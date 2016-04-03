;;;; jawbone-up.lisp

(in-package #:jawbone-up)

;;; "jawbone-up" goes here. Hacks and glory await!

(defun get-yesterday ()
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (- (get-universal-time)
				(* 24 60 60)))
    (declare (ignore s m h))
    (values day month year)))

(defun get-yesterday-date ()
  (multiple-value-bind (day month year)
      (get-yesterday)
    (+ day (* 100 month) (* 10000 year))))

(defun get-yesterday-steps ()
  (jsown:filter
   (car (jsown:filter (jsown:parse
		       (api/users/@me/moves :date (get-yesterday-date)))
		      "data" "items"))
   "details" "steps"))

(defun get-yesterday-km ()
  (jsown:filter
   (car (jsown:filter (jsown:parse
		       (api/users/@me/moves :date (get-yesterday-date)))
		      "data" "items"))
   "details" "km"))

(defun get-yesterday-active-time ()
  (jsown:filter
   (car (jsown:filter (jsown:parse
		       (api/users/@me/moves :date (get-yesterday-date)))
		      "data" "items"))
   "details" "active_time"))

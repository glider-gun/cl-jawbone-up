;;;; util.lisp

(in-package #:jawbone-up)

(export
 (defun save-to-png-file (path api-result)
   "Save a byte array `api-result` (assuming to be PNG) into `path`."
   (with-open-file (s path :direction :output
		      :element-type '(unsigned-byte 8)
		      :if-does-not-exist :create)
     (write-sequence api-result s))
   path))

(defun ensure-access-token-not-expired ()
  (assert *authentication-result*)
  (handler-bind
      ((dexador.error:http-request-unauthorized
	(lambda (c)
	  (print c)
	  (store-authentication-result
	   (get-access-token-by-refresh (refresh-token))))))
    (api/users/@me/moves)
    t))

(defun ensure-authorized ()
  (if *authentication-result*
      (ensure-access-token-not-expired)
      (if (restore-authentication-from-file)
	  (ensure-access-token-not-expired)
	  (authorize))))

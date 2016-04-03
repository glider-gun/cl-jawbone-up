;;;; auth.lisp

(in-package #:jawbone-up)


(defparameter *authentication-access-token-file*
  (merge-pathnames ".jawbone-up-access-token.json" (user-homedir-pathname))
  "a file where access token is saved into")

(defparameter *authentication-app-token-file*
  (merge-pathnames ".jawbone-up-app-token.json" (user-homedir-pathname))
  "a file where api key and secret are saved into")

;;; oauth client api key and secret
(defparameter *key* nil)
(defparameter *secret* nil)

(export
 (defun set-and-save-app-token (key secret)
   "set key and secret of for "
   (setf *key* key *secret* secret)  
   (with-open-file (s *authentication-app-token-file*
		      :direction :output
		      :if-exists :supersede
		      :if-does-not-exist :create)
     (print (list key secret) s))))


(defun restore-app-token-from-file ()
  (destructuring-bind (key secret)
      (with-open-file (s *authentication-app-token-file*)
	(read (read-all)))
    (setf *key* key *secret* secret)))

;;;; authentication of user

(defparameter *callback-port* 8888)
(defparameter *authentication-result* nil)
(defparameter *authentication-code* nil)

(defun authentication-url (&optional (scopes '("basic_read")))
  (let ((base-url "https://jawbone.com/auth/oauth2/auth")
	(params `(("response_type" . "code")
		  ("client_id" . ,*key*)
		  ("scope" . ,(format nil "~{~A~^ ~}" scopes))
		  ("redirect_uri" . ,(format nil "http://localhost:~D/access" *callback-port*)))))
    (concatenate 'string base-url "?" (quri.encode:url-encode-params params))))

;;;; server for call back
(defparameter *server* nil)
(defun start-acceptor (&optional verbose)
  (unless *server*
    (setf *server* (if verbose
		       (make-instance 'hunchentoot:easy-acceptor :port *callback-port*)
		       (make-instance 'hunchentoot:easy-acceptor :port *callback-port*
				      :message-log-destination nil
				      :access-log-destination nil)))
    (hunchentoot:start *server*)
    (hunchentoot:define-easy-handler (access :uri "/access") (code)
      (setf *authentication-code* code)
      (setf (hunchentoot:content-type*) "text/plain")
      (format nil "Authentication done! Now you can close this page."))))

(defun stop-acceptor ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)))

(defun get-token (code)
  (dex:post "https://jawbone.com/auth/oauth2/token"
	    :content `(("client_id" . ,*key*)
		       ("client_secret" . ,*secret*)
		       ("grant_type" . "authorization_code")
		       ("code" . ,code))))

(defun get-access-token-by-refresh (refresh-token)
  (dex:post "https://jawbone.com/auth/oauth2/token"
	    :content `(("client_id" . ,*key*)
		       ("client_secret" . ,*secret*)
		       ("grant_type" . "refresh_token")
		       ("refresh_token" . ,refresh-token))))

(defun store-authentication-result (result-json)
  (let ((result (jsown:parse result-json)))
    (setf *authentication-result* result)
    (with-open-file (s *authentication-access-token-file*
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (format s "~A~%" result-json))
    #+sbcl(sb-posix:chmod *authentication-access-token-file* #o600)
    result))

(defun read-all (s)
  (do ((str (read-line s nil nil) (read-line s nil nil))
       (strs nil (cons str strs)))
      ((null str)
       (format nil "~{~A~^~%~}" (nreverse strs)))))

(export
 (defun restore-authentication-from-file ()
   (when (probe-file *authentication-access-token-file*)
     (setf *authentication-result*
	   (with-open-file (s *authentication-access-token-file*)
	     (jsown:parse (read-all s)))))))

(export
 (defun authorize (&optional (scopes '("basic_read" "extended_read" "location_read" "friends_read" "mood_read" "mood_write" "move_read" "move_write" "sleep_read" "sleep_write" "meal_read" "meal_write" "weight_read" "weight_write" "generic_event_read" "generic_event_write" "heartrate_read")))
   (setf *authentication-code* nil)
   (start-acceptor)
   (trivial-open-browser:open-browser
    (authentication-url scopes))
   (do () (*authentication-code*)
     (sleep 0.1))
   (stop-acceptor)
   (let ((result-json (get-token *authentication-code*)))
     (store-authentication-result result-json))))

(defun access-token ()
  (jsown:val *authentication-result* "access_token"))

(defun refresh-token ()
  (jsown:val *authentication-result* "refresh_token"))


;;;; api.lisp

(in-package #:jawbone-up)

(defun api-url (api params)
  (concatenate 'string
	       "https://jawbone.com" api "?"
	       (quri.encode:url-encode-params params)))

(defun jb-request (method uri format-args json-parameters
		   &key (response-type "application/json") verbose)
  (apply #'dex:request (api-url (apply #'format nil uri format-args)
				(unless (eq method :post)
				  (remove nil json-parameters :key #'cdr)))
	 :method method
	 :headers `(("Accept" . ,response-type)
		    ("Authorization" . ,(format nil "Bearer ~A" (access-token))))
	 :verbose verbose
	 :content (when (eq method :post)
		    (remove nil json-parameters :key #'cdr))
	 nil))

(eval-when (:compile-toplevel :load-toplevel)
  (defun symbol-to-json-name (sym)
    (substitute #\_ #\- (string-downcase (symbol-name sym)))))

(defmacro defapi (name args json-parameter-keys &body body)
  "Define a function for jawbone api. `name` is the name of defined function.
Its argument are determined by `args` and `json-parameter-keys`.

  (defapi /foo/bar (xid) (date start-time)
    (buzz))

will be expanded as

  (defun /foo/bar (xid &key date start-time)
    (buzz))

defun form.



Inside `body`, a symbol %json-parameters% can be used.
this is expanded as a alist (in above example)

  ((\"date\" . date)
   (\"start_time\" . start-time))

which is intended to be passed as parameters for http request.
"
  `(prog1
       (defun ,name (,@args ,@(if json-parameter-keys
				(cons '&key json-parameter-keys)
				nil))
       ,@(when (stringp (car body))
	       (list (car body)))
       (let ((%json-parameters% (list
				 ,@(loop for k in json-parameter-keys
				      collect (list 'cons (symbol-to-json-name k)
						    k)))))
	 (declare (ignorable %json-parameters%))
	 ,@(if (stringp (car body))
	       (cdr body)
	       body)))))

(defmacro defapi-export (name args json-parameter-keys &body body)
  `(export
    (defapi ,name ,args ,json-parameter-keys ,@body)))


;;;;;;;;;;;;;;;;
;;;; band events

(defapi-export get/users/@me/bandevents () (date start-time end-time updated-after)
  "from https://jawbone.com/up/developer/endpoints/bandevents"
  (jb-request :get "/nudge/api/v.1.1/users/@me/bandevents" nil %json-parameters%))

;;;;;;;;;;;;;;;;
;;;; body events

(defapi-export get/users/@me/body_events () (date page-token start-time end-time updated-after limit)
  "from https://jawbone.com/up/developer/endpoints/body"
  (jb-request :get "/nudge/api/v.1.1/users/@me/body_events" nil %json-parameters%))

(defapi-export post/users/@me/body_events () (title weight body-fat lean-mass bmi note time-created tz share)
  "from https://jawbone.com/up/developer/endpoints/body"
  (jb-request :post "/nudge/api/v.1.1/users/@me/body_events" nil %json-parameters%))

(defapi-export get/body_events/{event_xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/body"
  (jb-request :get "/nudge/api/v.1.1/body_events/~A" (list xid) nil))

(defapi-export delete/body_events/{event_xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/body"
  (jb-request :delete "/nudge/api/v.1.1/body_events/~A" (list xid) nil))

;;;;;;;;;;;;;;;;
;;;; heart rate

(defapi-export get/users/@me/heartrates () (date page-token start-time end-time updated-after limit)
  "from https://jawbone.com/up/developer/endpoints/heartrate"
  (jb-request :get "/nudge/api/v.1.1/users/@me/heartrates" nil %json-parameters%))

;;;;;;;;;;;;;;;;
;;;; generic events

(defapi-export get/users/@me/generic_events ()
    (date page-token start-time end-time updated-after limit)
  "from https://jawbone.com/up/developer/endpoints/custom"
  (jb-request :get "/nudge/api/v.1.1/users/@me/generic_events" nil %json-parameters%))

(defapi-export post/users/@me/generic_events ()
    (title verb attributes note image-url place-lat place-lon place-acc place-name time-created tz share)
  "from https://jawbone.com/up/developer/endpoints/custom"
  (jb-request :post "/nudge/api/v.1.1/users/@me/generic_events" nil %json-parameters%))

(defapi-export post/generic_events/{xid}/partialUpdate (xid)
    (title verb attributes note image-url place-lat place-lon place-acc place-name time-created tz share)
  "from https://jawbone.com/up/developer/endpoints/custom"
  (jb-request :post "/nudge/api/v.1.1/generic_events/~A/partialUpdate" (list xid) %json-parameters%))

(defapi-export delete/generic_events/{xid} (xid)
    ()
  "from https://jawbone.com/up/developer/endpoints/custom"
  (jb-request :delete "/nudge/api/v.1.1/generic_events/~A" (list xid) nil))

;;;;;;;;;;;;;;;;
;;;; goals

(defapi-export get/users/@me/goals () ()
  "from https://jawbone.com/up/developer/endpoints/goals"
  (jb-request :get "/nudge/api/v.1.1/users/@me/goals" nil %json-parameters%))

(defapi-export post/users/@me/goals ()
    (move-steps sleep-total body-weight body-weight-intent)
  "from https://jawbone.com/up/developer/endpoints/goals"
  (jb-request :post "/nudge/api/v.1.1/users/@me/goals" nil %json-parameters%))

;;;;;;;;;;;;;;;;
;;;; meals

(defapi-export get/users/@me/meals () (date page-token start-time end-time updated-after)
  "from https://jawbone.com/up/developer/endpoints/meals"
  (jb-request :get "/nudge/api/v.1.1/users/@me/meals" nil %json-parameters%))

(defapi-export get/meals/{xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/meals"
  (jb-request :get "/nudge/api/v.1.1/meals/~A" (list xid) %json-parameters%))

(defapi-export post/users/@me/meals ()
    (note sub-type image-url photo place-lat place-lon place-acc place-name time-created tz share items)
  "from https://jawbone.com/up/developer/endpoints/meals"
  (jb-request :post "/nudge/api/v.1.1/users/@me/meals" nil %json-parameters%))

(defapi-export post/meals/{xid}/partialUpdate (xid)
    (note sub-type image-url photo place-lat place-lon place-acc place-name time-created tz share items)
  "from https://jawbone.com/up/developer/endpoints/meals"
  (jb-request :post "/nudge/api/v.1.1/meals/~A/partialUpdate" (list xid) %json-parameters%))

(defapi-export delete/meals/{xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/meals"
  (jb-request :delete "/nudge/api/v.1.1/meals/~A" (list xid) %json-parameters%))

;;;;;;;;;;;;;;;;
;;;; mood

(defapi-export get/users/@me/mood () (date)
  "from https://jawbone.com/up/developer/endpoints/mood"
  (jb-request :get "/nudge/api/v.1.1/users/@me/mood" nil %json-parameters%))

(defapi-export get/mood/{xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/mood"
  (jb-request :get "/nudge/api/v.1.1/mood/~A" (list xid) %json-parameters%))

(defapi-export post/users/@me/mood () (title sub-type time-created tz share)
  "from https://jawbone.com/up/developer/endpoints/mood"
  (jb-request :post "/nudge/api/v.1.1/users/@me/mood" nil %json-parameters%))

(defapi-export delete/mood/{xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/mood"
  (jb-request :delete "/nudge/api/v.1.1/mood/~A" (list xid) %json-parameters%))

;;;;;;;;;;;;;;;;
;;;; moves

(defapi-export get/users/@me/moves () (date page-token start-time end-time updated-after)
  "from https://jawbone.com/up/developer/endpoints/moves"
  (jb-request :get "/nudge/api/v.1.1/users/@me/moves" nil %json-parameters%))

(defapi-export get/moves/{move_xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/moves"
  (jb-request :get "/nudge/api/v.1.1/moves/~A" (list xid) nil))

(defapi-export get/moves/{move_xid}/image (xid) ()
  "from https://jawbone.com/up/developer/endpoints/moves"
  (jb-request :get "/nudge/api/v.1.1/moves/~A/image" (list xid) nil
	      :response-type "image/png"))

(defapi-export get/moves/{move_xid}/ticks (xid) ()
  "from https://jawbone.com/up/developer/endpoints/moves"
  (jb-request :get "/nudge/api/v.1.1/moves/~A/ticks" (list xid) nil))


;;;; refresh-token api is covered in auth.lisp

;;;;;;;;;;;;;;;;
;;;; settings

(defapi-export get/users/@me/settings () ()
  "from https://jawbone.com/up/developer/endpoints/settings"
  (jb-request :get "/nudge/api/v.1.1/users/@me/settings" nil %json-parameters%))

;;;;;;;;;;;;;;;;
;;;; sleeps

(defapi-export get/users/@me/sleeps () (date page-token start-time end-time updated-after)
  "from https://jawbone.com/up/developer/endpoints/sleeps"
  (jb-request :get "/nudge/api/v.1.1/users/@me/sleeps" nil %json-parameters%))

(defapi-export get/sleeps/{xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/sleeps"
  (jb-request :get "/nudge/api/v.1.1/sleeps/~A" (list xid) nil))

(defapi-export get/sleeps/{xid}/image (xid) ()
  "from https://jawbone.com/up/developer/endpoints/sleeps"
  (jb-request :get "/nudge/api/v.1.1/sleeps/~A/image" (list xid) nil
	      :response-type "image/png"))

(defapi-export get/sleeps/{xid}/ticks (xid) ()
  "from https://jawbone.com/up/developer/endpoints/sleeps"
  (jb-request :get "/nudge/api/v.1.1/sleeps/~A/ticks" (list xid) nil))


;;;;;;;;;;;;;;;;
;;;; time zone

(defapi-export get/users/@me/timezone () (start-time end-time date timestamp)
  "from https://jawbone.com/up/developer/endpoints/timezone"
  (jb-request :get "/nudge/api/v.1.1/users/@me/timezone" nil %json-parameters%))

;;;;;;;;;;;;;;;;
;;;; trends

(defapi-export get/users/@me/trends () (end-time bucket-size num-buckets)
  "from https://jawbone.com/up/developer/endpoints/trends"
  (jb-request :get "/nudge/api/v.1.1/users/@me/trends" nil %json-parameters%))

;;;;;;;;;;;;;;;;
;;;; user

(defapi-export get/users/@me () ()
  "from https://jawbone.com/up/developer/endpoints/user"
  (jb-request :get "/nudge/api/v.1.1/users/@me" nil %json-parameters%))

(defapi-export get/users/@me/friends () ()
  "from https://jawbone.com/up/developer/endpoints/user"
  (jb-request :get "/nudge/api/v.1.1/users/@me/friends" nil %json-parameters%))

;;;;;;;;;;;;;;;;
;;;; workouts

(defapi-export get/users/@me/workouts () (date page-token start-time end-time updated-after limit)
  "from https://jawbone.com/up/developer/endpoints/workouts"
  (jb-request :get "/nudge/api/v.1.1/users/@me/workouts" nil %json-parameters%))

(defapi-export get/workouts/{xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/workouts"
  (jb-request :get "/nudge/api/v.1.1/workouts/~A" (list xid) nil))

(defapi-export get/workouts/{xid}/image (xid) ()
  "from https://jawbone.com/up/developer/endpoints/workouts"
  (jb-request :get "/nudge/api/v.1.1/workouts/~A/image" (list xid) nil
	      :response-type "image/png"))

(defapi-export get/workouts/{xid}/ticks (xid) ()
  "from https://jawbone.com/up/developer/endpoints/workouts"
  (jb-request :get "/nudge/api/v.1.1/workouts/~A/ticks" (list xid) nil))

(defapi-export post/users/@me/workouts ()
    (sub-type time-created time-completed place-lat place-lon place-acc place-name tz share calories distance image-url intensity)
  "from https://jawbone.com/up/developer/endpoints/workouts"
  (jb-request :post "/nudge/api/v.1.1/users/@me/workouts" nil %json-parameters%))

(defapi-export post/workouts/{xid}/partialUpdate (xid)
    (sub-type time-created time-completed place-lat place-lon place-acc place-name tz share calories distance image-url intensity)
  "from https://jawbone.com/up/developer/endpoints/workouts"
  (jb-request :post "/nudge/api/v.1.1/workouts/~A/partialUpdate" (list xid) %json-parameters%))

(defapi-export delete/workouts/{xid} (xid) ()
  "from https://jawbone.com/up/developer/endpoints/workouts"
  (jb-request :delete "/nudge/api/v.1.1/workouts/~A" (list xid) nil))


;;;;;;;;;;;;;;;;
;;;; disconnect
(defapi-export get/users/@me/PartnerAppMembership () ()
  "from https://jawbone.com/up/developer/disconnection"
  (jb-request :delete "/nudge/api/users/@me/PartnerAppMembership" nil nil))


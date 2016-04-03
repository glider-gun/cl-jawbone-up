# What is this

A wrapper of [Jawbone UP](https://jawbone.com/up)'s Web API for personal use.
Please refer https://jawbone.com/up/developer/ about the APIs.

# usage:

1. Go the developer site https://jawbone.com/up/developer/ and sign in.
2. Select "Create App" from https://jawbone.com/up/developer/account

	during this, specify "http://localhost" to OAuth redirect URLs.
4. Load the library from lisp:

	clone this repository under whether

	- ~/quicklisp/local-projects

	  (if the lisp is installed without [roswell](https://github.com/roswell/roswell))
	- ~/.roswell/local-projects

	  (if the lisp is installed with [roswell](https://github.com/roswell/roswell))

	and do `(ql:quickload :jawbone-up)`.
5. Authenticate.

	- the first time after installed:
		- 1st: `(jawbone-up:set-and-save-app-token key secret)`

		  where key and secret are "Client Id" and "App Secret"
	      written in Application detail page. The key and secret are saved into a file specified by `jawbone-up:*authentication-app-token-file*`.

		- 2nd: `(jawbone-up:authorize)`

	      Browser automatically opens and you authorize the app at
          web. The access token is saved into a file specified by `jawbone-up:*authentication-access-token-file*`.
	- from next time (restore authentication info from files):
		- 1st: `(jawbone-up:restore-app-token-from-file)`
		- 2nd: `(jawbone-up:restore-authentication-from-file)`

6. Now you should be able to invoke apis.

	e.g.
   - `(jawbone-up:get/users/@me/moves)`
   - `(jawbone-up:get/users/@me/workouts)`

# notes
Almost all API results are returned as a json string.
You can use [jsown](https://github.com/madnificent/jsown) library or something to parse them like:

```lisp
;; get result on 2nd Apr. 2016
(jsown:parse
  (jawbone-up:get/users/@me/moves :date 20160402))

;; get number of step on 2nd Apr. 2016
(jsown:filter
  (car (jsown:filter
         (jsown:parse
           (api/users/@me/moves :date 20160402))
	       "data" "items"))
   "details" "steps")
```


Some APIs, whose name ends with "/image" (like `#'get/moves/{move_xid}/image`),
returns a binary data representing a png image.
For that case, you can use `#'jawbone-up:save-to-png-file` like:

```lisp
(jawbone-up:save-to-png-file "~/move.png"
  (jawbone-up:get/moves/{xid}/image "ABCDEFGHIJKLMNOPQRSTUVWXYQ012345"))
```


# Library API

## authentication
- external function `set-and-save-app-token` (key secret)

	Set application id and secret, and save it to file specified by `*authentication-app-token-file*`.
- external function `restore-authentication-from-file` ()

	Restore access token, from file specified by `*authentication-authentication-token-file*`.
- external function `restore-app-token-from-file` ()

	Restore application id and secret, from file specified in `*authentication-app-token-file*`.
- external function `authorize` (&optional
               (scopes
                '("basic\_read" "extended\_read" "location\_read" "friends\_read" "mood\_read"
                  "mood\_write" "move\_read" "move\_write" "sleep\_read" "sleep\_write"
                  "meal\_read" "meal\_write" "weight\_read" "weight\_write"
                  "generic\_event\_read" "generic\_event\_write" "heartrate\_read")))

	Do authentication to get access token, and save it to file specified by `*authentication-access-token-file*`. For the meaning of `scopes`, please refer to https://jawbone.com/up/developer/authentication . By default uses all scope.
- external variable `*authentication-app-token-file*`

	The path of file where api key and secret are saved into.
- external variable `*authentication-access-token-file*`

	The path of file where access token is saved into.

## misc
- external function `save-to-png-file`

	Save a byte array `api-result` (assuming to be PNG) into `path`.

## Web APIs
### band events: https://jawbone.com/up/developer/endpoints/bandevents
- external function `get/users/@me/bandevents` (&key date start-time end-time updated-after)

### body events: https://jawbone.com/up/developer/endpoints/body
- external function `get/users/@me/body_events` (&key date page-token start-time end-time
                               updated-after limit)
- external function `post/users/@me/body_events` (&key title weight body-fat lean-mass bmi note
                                time-created tz share)
- external function `get/body_events/{event_xid}` (xid)
- external function `delete/body_events/{event_xid}` (xid)

### heart rate: https://jawbone.com/up/developer/endpoints/heartrate
- external function `get/users/@me/heartrates` (&key date page-token start-time end-time
                              updated-after limit)

### custom events: https://jawbone.com/up/developer/endpoints/custom
- external function `get/users/@me/generic_events` (&key date page-token start-time end-time
                                  updated-after limit)
- external function `post/users/@me/generic_events` (&key title verb attributes note image-url
                                   place-lat place-lon place-acc place-name
                                   time-created tz share)
- external function `post/generic_events/{xid}/partialupdate` (xid &key title verb attributes
                                             note image-url place-lat place-lon
                                             place-acc place-name time-created
                                             tz share)
- external function `delete/generic_events/{xid}` (xid)

### goals: https://jawbone.com/up/developer/endpoints/goals
- external function `get/users/@me/goals` ()
- external function `post/users/@me/goals` (&key move-steps sleep-total body-weight
                          body-weight-intent)

### meals: https://jawbone.com/up/developer/endpoints/meals
- external function `get/users/@me/meals` (&key date page-token start-time end-time updated-after)
- external function `get/meals/{xid}` (xid)
- external function `post/users/@me/meals` (&key note sub-type image-url photo place-lat
                          place-lon place-acc place-name time-created tz share
                          items)
- external function `post/meals/{xid}/partialupdate` (xid &key note sub-type image-url photo
                                    place-lat place-lon place-acc place-name
                                    time-created tz share items)
- external function `delete/meals/{xid}` (xid)

### mood: https://jawbone.com/up/developer/endpoints/mood
- external function `get/users/@me/mood` (&key date)
- external function `get/mood/{xid}` (xid)
- external function `post/users/@me/mood` (&key title sub-type time-created tz share)
- external function `delete/mood/{xid}` (xid)

### moves: https://jawbone.com/up/developer/endpoints/moves
- external function `get/users/@me/moves` (&key date page-token start-time end-time updated-after)
- external function `get/moves/{move_xid}` (xid)
- external function `get/moves/{move_xid}/image` (xid)
- external function `get/moves/{move_xid}/ticks` (xid)

### settings: https://jawbone.com/up/developer/endpoints/settings
- external function `get/users/@me/settings` ()

### sleeps: https://jawbone.com/up/developer/endpoints/sleeps
- external function `get/users/@me/sleeps` (&key date page-token start-time end-time
                          updated-after)
- external function `get/sleeps/{xid}` (xid)
- external function `get/sleeps/{xid}/image` (xid)
- external function `get/sleeps/{xid}/ticks` (xid)

### time zone: https://jawbone.com/up/developer/endpoints/timezone
- external function `get/users/@me/timezone` (&key start-time end-time date timestamp)

### trends: https://jawbone.com/up/developer/endpoints/trends
- external function `get/users/@me/trends` (&key end-time bucket-size num-buckets)

### user: https://jawbone.com/up/developer/endpoints/user
- external function `get/users/@me` ()
- external function `get/users/@me/friends` ()

### workouts: https://jawbone.com/up/developer/endpoints/workouts
- external function `get/users/@me/workouts` (&key date page-token start-time end-time
                            updated-after limit)
- external function `get/workouts/{xid}` (xid)
- external function `get/workouts/{xid}/image` (xid)
- external function `get/workouts/{xid}/ticks` (xid)
- external function `post/users/@me/workouts` (&key sub-type time-created time-completed
                             place-lat place-lon place-acc place-name tz share
                             calories distance image-url intensity)
- external function `post/workouts/{xid}/partialupdate` (xid &key sub-type time-created
                                       time-completed place-lat place-lon
                                       place-acc place-name tz share calories
                                       distance image-url intensity)
- external function `delete/workouts/{xid}` (xid)
- external function `get/users/@me/partnerappmembership` ()

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

	  if the lisp is installed without [roswell](https://github.com/roswell/roswell)
	- ~/.roswell/local-projects (with roswell)

	  if the lisp is installed with [roswell](https://github.com/roswell/roswell)

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

# What is this

A wrapper for Jawbone UP's Web API for personal use.
Please refer https://jawbone.com/up/developer/ about the APIs.

# usage:

1. go the developer site https://jawbone.com/up/developer/ and sign in
2. select "Create App" from https://jawbone.com/up/developer/account

	during this, specify "http://localhost" to OAuth redirect URLs.
4. load `jawbone-up` from lisp:

	clone this repository under whether

	- ~/quicklisp/local-projects (without roswell)
	- ~/.roswell/local-projects (with roswell)

	and do `(ql:quickload :jawbone-up)`
5. authenticate.

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

6. now you should be able to invoke apis. e.g.
   - `(jawbone-up:get/users/@me/moves)`
   - `(jawbone-up:get/users/@me/workouts)`

# notes
Almost all API results are returned as a json string.
You can use `#'jsown:parse` or something to parse them.


Some APIs, whose name ends with "/image" (like `#'get/moves/{move_xid}/image`),
returns a binary data representing a png image.
For that case, you can use `#'jawbone-up:save-to-png-file` like:

```lisp
(jawbone-up:save-to-png-file "~/workout.png"
  (jawbone-up:get/workouts/{xid}/image "ABCDEFGHIJKLMNOPQRSTUVWXYQ012345"))
```

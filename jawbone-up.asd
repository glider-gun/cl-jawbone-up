;;;; jawbone-up.asd

(asdf:defsystem #:jawbone-up
  :description "jawbone-up api wrapper"
  :author "glidergun <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :version "0.0.1"
  :depends-on (:hunchentoot
	       :dexador
	       :quri
	       :trivial-open-browser
	       :jsown)
  :components ((:file "package")
               (:file "auth")
               (:file "api")
	       (:file "util")
               (:file "jawbone-up")))


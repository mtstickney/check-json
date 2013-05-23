;;;; check-json.asd

(asdf:defsystem #:check-json
  :serial t
  :description "Describe check-json here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-json)
  :components ((:file "package")
               (:file "check-json")))


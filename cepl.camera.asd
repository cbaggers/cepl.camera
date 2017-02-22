;;;; cepl.camera.asd

(asdf:defsystem #:cepl.camera
  :description "A camera implementation for CEPL"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:cepl #:rtg-math)
  :components ((:file "package")
               (:file "generics")
               (:file "base-camera")
               (:file "camera")))

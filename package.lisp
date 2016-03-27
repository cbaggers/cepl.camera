;;;; package.lisp

(defpackage #:cepl.camera.generics
  (:use :cl)
  (:export :viewport :cam->clip :x->cam))

(defpackage #:cepl.camera.base
  (:use :cl :cepl.generics :rtg-math
	:cepl.space #:cepl.camera.generics)
  (:shadowing-import-from :cepl.space :space)
  (:export :make-base-camera :update-cam->clip :base-camera
	   :with-base-camera :viewport :perspective :in-space :near :far :fov))

(defpackage #:cepl.camera
  (:use :cl :cepl.generics :rtg-math
	:cepl.space #:cepl.camera.generics
	#:cepl.camera.base)
  (:shadowing-import-from :cepl.space :space)
  (:export :make-camera :viewport :perspective :in-space :near :far :fov
	   :viewport :cam->clip :x->cam :using-camera
	   :camera-pos :camera-rot))

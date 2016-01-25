;;;; package.lisp

(defpackage #:cepl.camera.generics
  (:use :cl)
  (:export :viewport :cam->clip :x->cam))

(defpackage #:cepl.camera.base
  (:use :cl :cepl-generics :cl-game-math.base-vectors
	:jungl.space #:cepl.camera.generics)
  (:shadowing-import-from :jungl.space :space)
  (:import-from :cl-game-math.vector2
                :make-vector2)
  (:import-from :cl-game-math.vector3
                :make-vector3)
  (:import-from :cl-game-math.vector4
                :make-vector4)
  (:export :make-base-camera :update-cam->clip :base-camera
	   :with-base-camera :viewport :perspective :in-space :near :far :fov))

(defpackage #:cepl.camera
  (:use :cl :cepl-generics :cl-game-math.base-vectors
	:jungl.space #:cepl.camera.generics
	#:cepl.camera.base)
  (:shadowing-import-from :jungl.space :space)
  (:import-from :cl-game-math.vector2
                :make-vector2)
  (:import-from :cl-game-math.vector3
                :make-vector3)
  (:import-from :cl-game-math.vector4
                :make-vector4)
  (:export :make-camera :viewport :perspective :in-space :near :far :fov
	   :viewport :cam->clip :x->cam :using-camera))

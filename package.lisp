;;;; package.lisp

(defpackage #:cepl.camera
  (:use :cl :cepl-generics :cl-game-math.base-vectors
	:jungl.space)
  (:shadow :space)
  (:import-from :cl-game-math.vector2
                :make-vector2)
  (:import-from :cl-game-math.vector3
                :make-vector3)
  (:import-from :cl-game-math.vector4
                :make-vector4)
  (:export :make-camera))

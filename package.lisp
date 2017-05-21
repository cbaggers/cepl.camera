;;;; package.lisp

(uiop:define-package #:cepl.camera.generics
    (:use :cl :cepl :cepl.spaces)
  (:export :cam->clip :x->cam))

(uiop:define-package #:cepl.camera.base
    (:use :cl :cepl :rtg-math :cepl.spaces #:cepl.camera.generics)
  (:export :make-base-camera :update-cam->clip :base-camera
           :with-base-camera :perspective :in-space :near :far :fov
           :camera-viewport :camera-dimensions :camera-resolution
           :camera-fov))

(uiop:define-package #:cepl.camera
    (:use :cl :cepl :rtg-math :cepl.spaces #:cepl.camera.generics
          #:cepl.camera.base)
  (:export :camera :make-camera :perspective :in-space :near :far :fov
           :cam->clip :x->cam :using-camera :camera-pos :camera-rot
           :camera-viewport :camera-dimensions :camera-resolution
           :camera-fov)
  (:reexport :cepl.spaces))

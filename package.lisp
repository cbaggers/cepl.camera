;;;; package.lisp

(defpackage #:cepl.camera.generics
  (:use :cl :cepl)
  (:export :cam->clip :x->cam))

(defpackage #:cepl.camera.base
  (:use :cl :cepl :rtg-math :cepl.space #:cepl.camera.generics)
  (:export :make-base-camera :update-cam->clip :base-camera
           :with-base-camera :perspective :in-space :near :far :fov
           :camera-viewport))

(defpackage #:cepl.camera
  (:use :cl :cepl :rtg-math :cepl.space #:cepl.camera.generics
        #:cepl.camera.base)
  (:export :camera :make-camera :perspective :in-space :near :far :fov
           :cam->clip :x->cam :using-camera :camera-pos :camera-rot
           :camera-viewport))

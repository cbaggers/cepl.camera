;;;; package.lisp

(uiop:define-package #:cepl.camera.generics
    (:use :cl :cepl :cepl.spaces
           :varjo :varjo-lang)
  (:export :cam->clip :x->cam))

(uiop:define-package #:cepl.camera.base
    (:use :cl :cepl :rtg-math :cepl.spaces #:cepl.camera.generics
           :varjo :varjo-lang)
  (:export :make-base-camera :update-cam->clip :base-camera
           :with-base-camera :in-space :near :far :fov
           :camera-viewport :camera-dimensions :camera-resolution
           :camera-fov))

(uiop:define-package #:cepl.camera
    (:use :cl :cepl :rtg-math :cepl.spaces #:cepl.camera.generics
          #:cepl.camera.base :varjo :varjo-lang)
  (:import-from :cepl.camera.base :perspective)
  (:export :camera :make-camera :in-space :near :far :fov
           :cam->clip :x->cam :using-camera :camera-pos :camera-rot
           :camera-viewport :camera-dimensions :camera-resolution
           :camera-fov)
  (:reexport :cepl.spaces))

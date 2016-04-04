(in-package :cepl.camera.generics)

;;----------------------------------------------------------------------
;; Generic api
;;

(defgeneric cam->clip (camera)) ;; should return a mat4
(defgeneric x->cam (camera)) ;; should return a mat4

(in-package :cepl.camera)

;;----------------------------------------------------------------------
;; Generic api
;;

(defgeneric viewport (camera)) ;; should return a jungl:viewport
(defgeneric cam->clip (camera)) ;; should return a mat4

;;----------------------------------------------------------------------
;; Space Cameras
;;
;; Cepl's default camera implementation. It hooks into the space system
;; to provide better shader program experience if you are using the
;; 'jungl:space type.

;; Base - The base type for a camera that hooks into the space system
;;        Note that is does not provide as position or rotation so it does
;;        not handle calculating the world->camera transform. This let's you
;;        take ownership of this if you prefer.
;;

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Base

(defstruct base-camera-common
  (viewport (error "viewport must be supplied when making a camera")
	    :type jungl:viewport)
  (space (error "CEPL: Bug in cepl, space not provided when space-camera was created")
	 :type space)
  (in-space nil :type (or null space))
  (near 1.0 :type single-float)
  (far 1.0 :type single-float))

(defstruct (orthographic-base-camera (:include base-camera-common)))

(defstruct (perspective-base-camera (:include base-camera-common))
  (fov 120.0 :type single-float))

(defun make-base-camera (&key (viewport (jungl:current-viewport))
			   (projection :perspective)
			   (near 1.0)
			   (far 1000.0)
			   (fov 120.0))
  (case projection
    ((:perspective :p)
     (make-perspective-base-camera :space (if in-space
					      (space! *clip-space* in-space)
					      (space! *clip-space*))
				   :in-space nil
				   :viewport viewport
				   :near near
				   :far far
				   :fov fov))
    ((:orthographic :ortho :o)
     (make-orthographic-base-camera :space (if in-space
					       (space! *clip-space* in-space)
					       (space! *clip-space*))
				    :in-space nil
				    :viewport viewport
				    :near near
				    :far far))))

(defmacro with-base-camera (slots cam &body body)

  (assert (every (lambda (x) (member x '(viewport space near far fov in-space)))
		 slots))
  (labels ((get-base (x)
	     (if (eq x 'fov) 'perspective-base-camera 'base-camera-common)))
    `(symbol-macrolet
	 ,(mapcar (lambda (x)
		    `(,x '(,(utils:symb (get-base x) :- x) ,cam)))
		  slots)
       ,@body)))

(defun set-base-cam->clip (base-camera)
  (typecase base-camera
    (orthographic-base-camera
     (with-base-camera (viewport space near far) base-camera
       (let ((frame (jungl:viewport-resolution-v! viewport)))
	 (setf (get-transform space *clip-space*)
	       (cl-game-math.projection:orthographic
		(v:x frame) (v:y frame) near far)))))
    (perspective-base-camera
     (with-base-camera (viewport space near far fov) base-camera
       (let ((frame (jungl:viewport-resolution-v! viewport)))
	 (setf (get-transform space *clip-space*)
	       (cl-game-math.projection:perspective
		(v:x frame) (v:y frame) near far fov)))))))

(defmethod viewport ((camera base-camera-common))
  (base-camera-common-viewport camera))

(defmethod cam->clip ((camera base-camera-common))
  (with-base-camera (in-space space) camera
    (if in-space
	(get-transform in-space space)
	(get-transform space))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defstruct (orthographic-space-camera (:include orthographic-base-camera))
  (pos (v! 0 0 0 0) :type cl-game-math.types:vec4)
  (rot (q:identity-quat) :type cl-game-math.types:quaternion))

(defstruct (perspective-space-camera (:include orthographic-base-camera))
  (pos (v! 0 0 0 0) :type cl-game-math.types:vec4)
  (rot (q:identity-quat) :type cl-game-math.types:quaternion))

(defmethod print-object ((cam orthographic-space-camera) stream)
  (format stream "#<CAMERA :ORTHO ~s>"))



(defun make-camera (&key (viewport (jungl:current-viewport))
		      (in-space *world-space*)
		      (projection :perspective)
		      (near 1.0)
		      (far 1000.0)
		      (fov 120.0))
  (case projection
    ((:perspective :p)
     (make-perspective-base-camera :space (if in-space
					      (space! *clip-space* in-space)
					      (space! *clip-space*))
				   :in-space in-space
				   :viewport viewport
				   :near near
				   :far far
				   :fov fov))
    ((:orthographic :ortho :o)
     (make-orthographic-base-camera :space (if in-space
					       (space! *clip-space* in-space)
					       (space! *clip-space*))
				    :in-space in-space
				    :viewport viewport
				    :near near
				    :far far))))

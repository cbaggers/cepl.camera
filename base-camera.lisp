(in-package :cepl.camera.base)

;;----------------------------------------------------------------------
;; Space Cameras
;;
;; Base - The base type for a camera that hooks into the space system
;;        Note that is does not provide as position or rotation so it does
;;        not handle calculating the world->camera transform. This let's you
;;        take ownership of this if you prefer.
;;

(defstruct (base-camera (:constructor %make-base-camera))
  (viewport (error "viewport must be supplied when making a camera")
	    :type jungl:viewport)
  (space (error "CEPL: Bug in cepl, space not provided when space-camera was created")
	 :type jungl.space:space)
  (perspective t :type boolean)
  (in-space nil :type (or null jungl.space:space))
  (near 1.0 :type single-float)
  (far 1.0 :type single-float)
  (fov 120.0 :type single-float))

(defun make-base-camera (&key (viewport (jungl:current-viewport))
			   (projection :perspective)
			   (near 1.0)
			   (in-space *world-space*)
			   (far 1000.0)
			   (fov 120.0))
  (%make-base-camera :space (if in-space
				(space! *clip-space* in-space)
				(space! *clip-space*))
		     :perspective (ecase projection
				    ((:perspective :p) t)
				    ((:orthographic :ortho :o) nil))
		     :in-space nil
		     :viewport viewport
		     :near near
		     :far far
		     :fov fov))

(defmacro with-base-camera (slots cam &body body)

  (assert (every (lambda (x) (member x '(viewport space near far fov in-space
					 perspective)))
		 slots))
  `(symbol-macrolet
       ,(mapcar (lambda (x)
		  `(,x (,(utils:symb-package :cepl.camera.base :base-camera- x)
			 ,cam)))
		slots)
     ,@body))

(defun update-cam->clip (base-camera)
  (with-base-camera (viewport space near far fov perspective) base-camera
    (let ((frame (jungl:viewport-resolution-v! viewport)))
      (setf (get-transform space *clip-space*)
	    (if perspective
		(cl-game-math.projection:perspective (v:x frame) (v:y frame)
						     near far fov)
		(cl-game-math.projection:orthographic (v:x frame) (v:y frame)
						      near far))))))


(defmethod viewport ((camera base-camera))
  (base-camera-viewport camera))

(defmethod cam->clip ((camera base-camera))
  (with-base-camera (space) camera
    (get-transform space *clip-space*)))

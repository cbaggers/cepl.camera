(in-package :cepl.camera)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar *uids* -1)

(defstruct (camera (:include base-camera)
		   (:constructor %make-camera)
		   (:conc-name %camera-))
  (uid (incf *uids*) :type fixnum)
  (pos (v! 0 0 0 0) :type rtg-math.types:vec4)
  (rot (q:identity-quat) :type rtg-math.types:quaternion))

(defmethod print-object ((cam camera) stream)
  (with-base-camera (perspective) cam
    (format stream "#<CAMERA (~s) ~s>"
	    (%camera-uid cam)
	    (if perspective :perspective :ortho))))

(defun update-x->cam (camera)
  (with-base-camera (in-space space) camera
    (setf (get-transform in-space space)
	  (m4:m* (m4:translation (v4:negate (%camera-pos camera)))
		 (q:to-matrix4
		  (q:normalize
		   (%camera-rot camera)))))))

(defun make-camera (&key
		      (pos (v! 0 0 0 0))
		      (rot (q:identity-quat))
		      (viewport (jungl:clone-viewport (jungl:current-viewport)))
		      (in-space *world-space*)
		      (projection :perspective)
		      (near 1.0)
		      (far 1000.0)
		      (fov 120.0))
  (unless in-space
    (error "Cepl.Camera: Space is mandatory when constructing camera"))
  (let ((cam (%make-camera :space (make-space* *clip-space* in-space)
			   :perspective (ecase projection
					  ((:perspective :p) t)
					  ((:orthographic :ortho :o) nil))
			   :in-space in-space
			   :viewport viewport
			   :near near
			   :far far
			   :fov fov
			   :pos pos
			   :rot rot)))
    (update-x->cam cam)
    (update-cam->clip cam)
    cam))

(defun camera-pos (camera)
  (%camera-pos camera))

(defun camera-rot (camera)
  (%camera-rot camera))

(defun (setf camera-pos) (value camera)
  (setf (%camera-pos camera) value)
  (update-x->cam camera)
  (update-cam->clip camera)
  value)

(defun (setf camera-rot) (value camera)
  (setf (%camera-rot camera) value)
  (update-x->cam camera)
  (update-cam->clip camera)
  value)

(defmacro using-camera (camera &body body)
  (destructuring-bind (camera &key (safe t))
      (cepl-utils:listify camera)
    (let ((space (gensym "space"))
	  (cam (gensym "cam")))
      `(let ((,cam ,camera))
	 ,@(when safe
		 `((unless (typep ,cam 'camera)
		     (error "Cannot render using this type as a camera: ~s" ,cam))))
	 (let ((,space (cepl.camera.base::base-camera-space ,cam)))
	   (with-rendering-via ,space
	     (jungl::with-viewport (cepl.camera.base::base-camera-viewport ,cam)
	       ,@body)))))))

(defmethod x->cam ((camera camera))
  (with-base-camera (space in-space) camera
    (get-transform in-space space)))

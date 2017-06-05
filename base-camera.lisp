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
            :type viewport)
  (space (error "CEPL: Bug in cepl, space not provided when space-camera was created")
         :type cepl.spaces:vec-space)
  (perspective t :type boolean)
  (in-space nil :type (or null cepl.spaces:vec-space))
  (near 1.0 :type single-float)
  (far 1.0 :type single-float)
  (fov 90.0 :type single-float))

(defun make-base-camera (&key (viewport (current-viewport))
                           (projection :perspective)
                           (near 1.0)
                           (in-space *world-space*)
                           (far 1000.0)
                           (fov 120.0))
  (%make-base-camera :space (if in-space
                                (make-space* *clip-space* in-space)
                                (make-space* *clip-space*))
                     :perspective (ecase projection
                                    ((:perspective :p) t)
                                    ((:orthographic :ortho :o) nil))
                     :in-space nil
                     :viewport viewport
                     :near near
                     :far far
                     :fov fov))

(defmacro with-base-camera (slots cam &body body)
  (assert (every (lambda (x)
                   (member x '(:viewport :space :near :far :fov :in-space :perspective)
                           :test #'string=))
                 slots))
  `(symbol-macrolet
       ,(mapcar (lambda (x)
                  `(,x (,(cepl-utils:symb-package :cepl.camera.base :base-camera- x)
                         ,cam)))
                slots)
     ,@body))

(defun update-cam->clip (base-camera)
  (with-base-camera (viewport space near far fov perspective) base-camera
    (let ((frame (viewport-resolution viewport)))
      (setf (get-transform space *clip-space*)
            (if perspective
                (rtg-math.projection:perspective (v:x frame) (v:y frame)
                                                 near far fov)
                (rtg-math.projection:orthographic (v:x frame) (v:y frame)
                                                  near far))))))

(defun camera-fov (camera)
  (base-camera-fov camera))

(defgeneric fov (camera))
(defgeneric (setf fov) (val camera))

(defmethod fov ((camera base-camera))
  (base-camera-fov camera))

(defun (setf camera-fov) (value camera)
  (prog1 (setf (base-camera-fov camera) value)
    (update-cam->clip camera)))

(defmethod (setf fov) (value (camera base-camera))
  (prog1 (setf (base-camera-fov camera) value)
    (update-cam->clip camera)))

(defun camera-viewport (camera)
  (base-camera-viewport camera))

(defmethod viewport ((camera base-camera))
  (base-camera-viewport camera))

(defmethod (setf viewport) (value (camera base-camera))
  (assert (typep value 'viewport))
  (setf (base-camera-viewport camera) value))

(defmethod cam->clip ((camera base-camera))
  (with-base-camera (space) camera
    (get-transform space *clip-space*)))

(defun camera-dimensions (camera)
  (cepl:viewport-dimensions (camera-viewport camera)))

(defun camera-resolution (camera)
  (cepl:viewport-resolution (camera-viewport camera)))

(defun (setf camera-dimensions) (new-dimensions camera)
  (prog1 (setf (cepl:viewport-dimensions (camera-viewport camera))
               new-dimensions)
    (update-cam->clip camera)))

(defun (setf camera-resolution) (new-resolution-v2 camera)
  (prog1 (setf (cepl:viewport-resolution (camera-viewport camera))
               new-resolution-v2)
    (update-cam->clip camera)))

(defmethod resolution ((camera base-camera))
  (camera-resolution camera))

(defmethod (setf resolution) (value (camera base-camera))
  (setf (camera-resolution camera) value))

(defmethod dimensions ((camera base-camera))
  (camera-dimensions camera))

(defmethod (setf dimensions) (value (camera base-camera))
  (setf (camera-dimensions camera) value))

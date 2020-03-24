;;;; vvm.lisp

(defpackage #:vvm
  (:use #:cl))

(in-package #:vvm)

;;; Matrix ops

(defstruct (transform-matrix
             (:type vector)
             (:constructor matrix (x-scale y-skew x-skew y-scale x-offset y-offset)))
 (x-scale 1.0)
 (y-skew 0.0)
 (x-skew 0.0)
 (y-scale 1.0)
 (x-offset 0.0)
 (y-offset 0.0))

(defmacro matrix-bind (lambda-list vector &body body)
  (when (/= (length lambda-list) 6)
    (error "Bad lambda-list for MATRIX-BIND: 6 arguments required"))
  (let ((vec (gensym)))
  `(let ((,vec ,vector))
     (let (,@(loop for i from 0 below 6
                  for var in lambda-list
                  collect (list var `(aref ,vec ,i))))
       ,@body))))


(defun make-transform-function (transform-matrix)
  (matrix-bind (a b c d e f)
      transform-matrix
    (lambda (x y)
      (values (+ (* a x) (* c y) e)
              (+ (* b x) (* d y) f)))))

(defun transform-coordinates (x y transform-matrix)
  (matrix-bind (a b c d e f)
      transform-matrix
    (values (+ (* a x) (* c y) e)
            (+ (* b x) (* d y) f))))

(defun mult (m1 m2)
  (matrix-bind (a b c d e f)
      m1
    (matrix-bind (a* b* c* d* e* f*)
        m2
      (matrix (+ (* a a*)
                 (* b c*))
              (+ (* a b*)
                 (* b d*))
              (+ (* c a*)
                 (* d c*))
              (+ (* c b*)
                 (* d d*))
              (+ (* e a*)
                 (* f c*)
                 e*)
              (+ (* e b*)
                 (* f d*)
                 f*)))))

(defun nmult (m1 m2)
  "Destructive MULT; M2 is modified to hold the result of multiplication."
  (matrix-bind (a b c d e f)
      m1
    (matrix-bind (a* b* c* d* e* f*)
        m2
      (setf (aref m2 0)
            (+ (* a a*)
               (* b c*))
            (aref m2 1)
            (+ (* a b*)
               (* b d*))
            (aref m2 2)
            (+ (* c a*)
               (* d c*))
            (aref m2 3)
            (+ (* c b*)
               (* d d*))
            (aref m2 4)
            (+ (* e a*)
               (* f c*)
               e*)
            (aref m2 5)
            (+ (* e b*)
               (* f d*)
               f*))
      m2)))

(defun translation-matrix (tx ty)
  (matrix 1 0 0 1 tx ty))

(defun scaling-matrix (sx sy)
  (matrix sx 0 0 sy 0 0))

(defun rotation-matrix (theta)
  (let ((cos (cos theta))
        (sin (sin theta)))
    (matrix cos sin (- sin) cos 0 0)))

(defun skewing-matrix (alpha beta)
  (matrix 1 (tan alpha) (tan beta) 1 0 0))

(defun identity-matrix ()
  (matrix 1.0 0.0 0.0 1.0 0.0 0.0))

(defun invert-matrix (matrix)
  (matrix-bind (a11 a12 a21 a22 a31 a32)
      matrix
    (let ((a13 0)
          (a23 0)
          (a33 1))
      (let* ((a33a22 (* a33 a22))
             (a32a23 (* a32 a23))
             (a33a12 (* a33 a12))
             (a32a13 (* a32 a13))
             (a23a12 (* a23 a12))
             (a22a13 (* a22 a13))
             (determinant
              ;; a11(a33a22-a32a23)-a21(a33a12-a32a13)+a31(a23a12-a22a13)
              (- (* a11 (- a33a22 a32a23))
                 (+ (* a21 (- a33a12 a32a13))
                    (* a31 (- a23a12 a22a13))))))
        ;;   a33a22-a32a23  -(a33a12-a32a13)
        ;; -(a33a21-a31a23)   a33a11-a31a13
        ;;   a32a21-a31a22  -(a32a11-a31a12)
        (let ((a (- a33a22 a32a23))
              (b (- (- a33a12 a32a13)))
              (c (- (- (* a33 a21) (* a31 a23))))
              (d (- (* a33 a11) (* a31 a13)))
              (e (- (* a32 a21) (* a31 a22)))
              (f (- (- (* a32 a11) (* a31 a12)))))
          (matrix (/ a determinant)
                  (/ b determinant)
                  (/ c determinant)
                  (/ d determinant)
                  (/ e determinant)
                  (/ f determinant)))))))


;;; Text stuff

(defclass font ()
  ((loader
    :initarg :loader
    :accessor loader)
   (transform-matrix
    :initarg :transform-matrix
    :accessor transform-matrix)
   (size
    :initarg :size
    :accessor size)))

(defmethod slot-unbound ((class t) (font font) (slot-name (eql 'transform-matrix)))
  (let ((scale (loader-font-scale (size font) (loader font))))
    (setf (slot-value font 'transform-matrix)
          (scaling-matrix scale scale))))

(defun draw-glyph-primitive (glyph transform-matrix)
  (let ((transformer (make-transform-function transform-matrix)))
    (flet ((p (font-point)
             (funcall transformer
                      (zpb-ttf:x font-point)
                      (zpb-ttf:y font-point))))
      (zpb-ttf:do-contours (contour glyph)
        (when (plusp (length contour))
          (let ((first-point (aref contour 0)))
            (multiple-value-call #'move-to/ll (p first-point))
            (zpb-ttf:do-contour-segments* (control end)
                contour
              (if control
                  (multiple-value-call #'quadratic-to/ll
                    (p control)
                    (p end))
                  (multiple-value-call #'line-to/ll (p end))))))))))

(defun string-glyphs (string loader)
  "Return STRING converted to a list of ZPB-TTF glyph objects from FONT."
  (map 'list (lambda (char) (zpb-ttf:find-glyph char loader)) string))

(defun draw-string-primitive (x y string font &key (character-spacing 1.0d0))
  "Draw STRING, transformed by the font scale of FONT."
  (let ((glyphs (string-glyphs string (loader font)))
        (loader (loader font))
        (matrix (mult (transform-matrix font) (translation-matrix x y))))
    (loop for (glyph . rest) on glyphs do
          (draw-glyph-primitive glyph matrix)
          (when rest
            (let* ((next (first rest))
                   (offset (+ (zpb-ttf:advance-width glyph)
                              (zpb-ttf:kerning-offset glyph next loader))))
              (setf matrix (nmult (translation-matrix (* offset
                                                         character-spacing)
                                                      0)
                                  matrix)))))))

(defun loader-font-scale (size loader)
  "Return the horizontal and vertical scaling needed to draw the
glyphs of LOADER at SIZE units."
  (float (/ size (zpb-ttf:units/em loader))))

(defun call-with-font (fun font-file)
  (zpb-ttf:with-font-loader (loader font-file)
    (funcall fun loader)))


;;; Geometry stuff

(defstruct (box (:type (vector (complex double-float)))
                (:conc-name ""))
  (minpoint #c (0.0d0 0.0d0) :type (complex double-float))
  (maxpoint #c (0.0d0 0.0d0) :type (complex double-float)))

(defun box (xmin ymin xmax ymax)
  (make-box :minpoint (point xmin ymin)
            :maxpoint (point xmax ymax)))

(defun point-box (a b)
  (box (min (x a) (x b)) (min (y a) (y b))
       (max (x a) (x b)) (max (y a) (y b))))

(defun xmin (box)
  (x (minpoint box)))

(defun xmax (box)
  (x (maxpoint box)))

(defun ymin (box)
  (y (minpoint box)))

(defun ymax (box)
  (y (maxpoint box)))

(defun midpoint (a b)
  (point (/ (+ (x a) (x b)) 2.0d0)
           (/ (+ (y a) (y b)) 2.0d0)))

(defun centerpoint (box)
  (midpoint (minpoint box) (maxpoint box)))


(defun apoint (angle distance)
  (point (* distance (cos angle))
         (* distance (sin angle))))

(defun angle (p1 p2)
  (let* ((diff (sub p2 p1))
         (x (x diff))
         (y (y diff)))
    (if (zerop x)
        (if (plusp y)
            (/ pi 2)
            (* 3 (/ pi 2)))
        (atan y x))))

(macrolet ((define-point-op (name operation)
             `(defun ,name (a b)
                (point (,operation (x a) (x b))
                       (,operation (y a) (y b))))))
  (define-point-op add/2 +)
  (define-point-op sub -)
  (define-point-op mul *)
  (define-point-op div /))

(defun add (a b &rest args)
  (if args
      (reduce #'add/2 args :initial-value (add/2 a b))
      (add/2 a b)))

(defun distance (p1 p2)
  (let ((diff (sub p1 p2)))
    (sqrt (+ (* (x diff) (x diff))
             (* (y diff) (y diff))))))

(defun width (object)
  (- (xmax object) (xmin object)))

(defun height (object)
  (- (ymax object) (ymin object)))

(defun area (object)
  (* (height object) (width object)))

(defun emptyp (box)
  (= (minpoint box) (maxpoint box)))


(defgeneric bounding-box (object)
  (:method ((object vector))
    (assert (typep object '(vector (complex double-float) 2)))
    object))

(defgeneric top-left (object)
  (:method (object)
    (let ((box (bounding-box object)))
      (point (xmin box) (ymax box)))))

(defgeneric top-right (object)
  (:method (object)
    (maxpoint (bounding-box object))))

(defgeneric bottom-left (object)
  (:method (object)
    (minpoint (bounding-box object))))

(defgeneric bottom-right (object)
  (:method (object)
    (let ((box (bounding-box object)))
      (point (xmax box) (ymin box)))))

(macrolet ((compass-point-method (name component1 &optional component2)
             (if component2
                 `(defgeneric ,name (object)
                    (:method (object)
                      (midpoint (,component1 object)
                                (,component2 object))))
                 `(defgeneric ,name (object)
                    (:method (object)
                      (,component1 object))))))
  (compass-point-method northpoint top-left top-right)
  (compass-point-method northeastpoint top-right)
  (compass-point-method eastpoint top-right bottom-right)
  (compass-point-method southeastpoint bottom-right)
  (compass-point-method southpoint bottom-left bottom-right)
  (compass-point-method southwestpoint bottom-left)
  (compass-point-method westpoint bottom-left top-left)
  (compass-point-method northwestpoint top-left))

(defun contract (box amount)
  (let ((p (point amount amount)))
    (point-box (add (minpoint box) p)
               (sub (maxpoint box) p))))

(defun expand (box amount)
  (contract box (- amount)))

(defvar *origin* (point 0 0))



;;; Color stuff

(defstruct (rgba-color (:type (vector double-float))
                       (:conc-name ""))
  (red 1.0d0 :type double-float)
  (green 1.0d0 :type double-float)
  (blue 1.0d0 :type double-float)
  (alpha 1.0d0 :type double-float))

(defun rgb-color (r g b)
  (make-rgba-color :red (float r 1.0d0)
                   :green (float g 1.0d0)
                   :blue (float b 1.0d0)))

(defun rgba-color (r g b a)
  (make-rgba-color :red (float r 1.0d0)
                   :green (float g 1.0d0)
                   :blue (float b 1.0d0)
                   :alpha (float a 1.0d0)))

(defun rgb->hsv (r g b)
  (let* ((min (min r g b))
         (max (max r g b))
         (delta (- max min))
         (v max)
         (s 0)
         (h nil))
    (when (plusp max)
      (setq s (/ delta max)))
    (when (plusp delta)
      (setq h (cond
               ((= max r)
                (nth-value 0 (/ (- g b) delta)))
               ((= max g)
                (nth-value 0 (+ 2 (/ (- b r) delta))))
               (t
                (nth-value 0 (+ 4 (/ (- r g) delta))))))
      (setq h (* 60 h))
      (when (minusp h)
        (incf h 360)))
    (values h s v)))

(defun hsv->rgb (h s v)
  (when (zerop s)
    (return-from hsv->rgb (values v v v)))

  (loop while (minusp h)
        do (incf h 360))
  (loop while (>= h 360)
        do (decf h 360))

  (let ((h-pos (/ h 60)))
    (multiple-value-bind (h-int h-frac) (truncate h-pos)
      (declare (fixnum h-int))
      (let ((p (* v (- 1 s)))
            (q (* v (- 1 (* s h-frac))))
            (t_ (* v (- 1 (* s (- 1 h-frac)))))
            r g b)

        (cond
         ((zerop h-int)
          (setf r v
                g t_
                b p))
         ((= 1 h-int)
          (setf r q
                g v
                b p))
         ((= 2 h-int)
          (setf r p
                g v
                b t_))
         ((= 3 h-int)
          (setf r p
                g q
                b v))
         ((= 4 h-int)
          (setf r t_
                g p
                b v))
         ((= 5 h-int)
          (setf r v
                g p
                b q)))
        (values r g b)))))

(defun hsv-color (h s v)
  (multiple-value-call 'rgb-color (hsv->rgb h s v)))

(defun hsv-values (color)
  (rgb->hsv (red color) (green color) (blue color)))

(defun rgb-values (color)
  (values (red color) (green color) (blue color)))

(defvar *black* (rgb-color 0 0 0))
(defvar *white* (rgb-color 1 1 1))

(defun darkp (color)
  (multiple-value-bind (hue saturation value)
      (hsv-values color)
    (or (< value 0.64)
        (and (< 0.5 saturation)
             (or (< hue 45) (< 205 hue))))))

(defun contrasting-text-color (color)
  (if (darkp color)
      *white*
      *black*))

(defun add-alpha (color alpha)
  (multiple-value-call #'rgba-color (rgb-values color) alpha))

(defun float-octet (float)
  "Convert a float in the range 0.0 - 1.0 to an octet."
  (values (round (* float 255.0d0))))

(defun gray-color (value)
  (rgb-color value value value))

(defun graya-color (value alpha)
  (rgba-color value value value alpha))

;;; Graphics programs

(defvar *opdb* (make-hash-table))
(defclass op ()
  ((name
    :reader name
    :initarg :name)
   (args
    :reader args
    :initarg :args)))

(defmethod print-object ((op op) stream)
  (print-unreadable-object (op stream :type t)
    (format stream "~S~{ ~S~}"
            (name op)
            (args op))))

(defun op (name)
  (or (gethash name *opdb*)
      (error "Unknown op ~S" name)))

(defun arity (op)
  (length (args op)))

(defvar *program*)

(defun emit (op &rest args)
  (vector-push-extend (list* op args) *program*))

(defmacro define-op (operator &rest arguments)
  (labels ((argument-type (arg)
             (if (consp arg)
                 (second arg)
                 arg))
           (argument-name (arg)
             (if (consp arg)
                 (first arg)
                 arg))
           (newsym (&rest syms)
             (intern (format nil "~{~A~}"
                             (mapcar 'symbol-name syms))))
           (symsuffixes (sym &rest suffixes)
             (mapcar (lambda (suffix) (newsym sym suffix))
                     suffixes))
           (argument-low-level-args (arg)
             (let ((name (argument-name arg))
                   (type (argument-type arg)))
               (ecase type
                 (point
                  (symsuffixes name :-x :-y))
                 (color
                  (symsuffixes name :-red :-green :-blue :-alpha))
                 ((line-cap-style line-join-style line-width
                                  radians number string vector)
                  (list name)))))
           (argument-expander (arg)
             (let ((name (argument-name arg))
                   (type (argument-type arg)))
               (case type
                 (point `(point-components ,name))
                 (color `(color-rgba-components ,name))
                 (t name)))))
    (let ((low-level (mapcan #'argument-low-level-args arguments))
          (high-level (mapcar #'argument-name arguments))
          (low-level-fun (newsym operator :/ll)))
      `(progn
         (setf (gethash ',operator *opdb*)
               (make-instance 'op
                              :name ',operator
                              :args ',low-level))
         (defun ,low-level-fun ,low-level
           (emit ',operator ,@low-level))
         (defun ,operator ,high-level
           (multiple-value-call ',low-level-fun ,@ (mapcar #'argument-expander arguments)))))))

(defun point-components (point)
  (values (realpart point)
          (imagpart point)))

(defun x (point) (realpart point))
(defun y (point) (imagpart point))

(defun color-rgba-components (color)
  (values (aref color 0)
          (aref color 1)
          (aref color 2)
          (aref color 3)))

(defun call-with-program (fun)
  (let ((*program* (make-array 32 :fill-pointer 0 :adjustable t)))
    (funcall fun)
    *program*))

(defmacro with-program (&body body)
  `(call-with-program (lambda () ,@body)))

(define-op move-to point)
(define-op line-to point)
(define-op curve-to (control1 point) (control2 point) (end point))
(define-op quadratic-to (control point) (end point))
(define-op arc
    (center point) (radius number) (theta1 radians) (theta2 radians))
(define-op arcn
    (center point) (radius number) (theta1 radians) (theta2 radians))
(define-op ellipse-arc
    (center point) (radius number)
    (theta radians) (eta1 radians) (eta2 radians))
(define-op ellipse-arcn
    (center point) (radius number)
    (theta radians) (eta1 radians) (eta2 radians))

(define-op close-subpath)
(define-op end-path-no-op)

(define-op clip-path)
(define-op even-odd-clip-path)

(define-op fill-path)
(define-op stroke)
(define-op stroke-to-paths)
(define-op fill-and-stroke)
(define-op even-odd-fill)
(define-op even-odd-fill-and-stroke)
(define-op clear-canvas)

(define-op translate point)
(define-op scale point)
(define-op skew point)
(define-op rotate radians)

(define-op set-dash-pattern vector (phase radians))
(define-op set-line-cap line-cap-style)
(define-op set-line-join line-join-style)
(define-op set-line-width (width number))
(define-op set-fill-color color)
(define-op set-stroke-color color)

(define-op draw-string point string)
(define-op draw-centered-string (point string))

(define-op save-graphics-state)
(define-op restore-graphics-state)

(define-op set-font (font string))
(define-op get-font (font string))
(define-op set-font-size (size number))
(define-op set-font-character-spacing (factor number))

(defvar *origin* (complex 0.0d0 0.0d0))

(defun point (x y)
  (complex (float x 1.0d0) (float y 1.0d0)))


(defmacro op-ecase (op &body clauses)
  (let ((operator (gensym "OPERATOR"))
        (opsym (copy-symbol 'op)))
    `(let* ((,opsym ,op)
            (,operator (first ,opsym)))
       (ecase ,operator
         ,@(loop for ((target . args) . body) in clauses
                 collect `(,target
                           (destructuring-bind (,@args)
                               (rest ,opsym)
                             ,@body)))))))

(defun program-bounding-box (program)
  (let (xmin ymin xmax ymax)
    (flet ((update (x y)
             (setf xmin (min x (or xmin x))
                   ymin (min y (or ymin y))
                   xmax (max x (or xmax x))
                   ymax (max y (or ymax y)))))
      (loop for op across program
            do
            (op-ecase op
              ((move-to x y)
               (update x y))
              ((line-to x y)
               (update x y))
              ((curve-to cx1 cy1 cx2 cy2 x y)
               (update cx1 cy1)
               (update cx2 cy2)
               (update x y))
              ((quadratic-to cx cy x y)
               (update cx cy)
               (update x y))))
      (values xmin ymin xmax ymax))))

(defun testme ()
  (with-program
    (rotate (/ pi 4))
    (call-with-font (lambda (font)
                      (draw-string-primitive 0 0 "Hello"
                                             (make-instance 'font
                                                            :loader font
                                                            :size 48)))
                    "/Users/xach/times.ttf")
    (fill-path)))

(defun centered-circle-path/ll (cx cy radius)
  (emit 'arc cx cy radius 0 (* pi 2))
  (emit 'close-subpath))

(defun centered-circle-path (center-point radius)
  (multiple-value-call #'centered-circle-path/ll
    (point-components center-point)
    radius))

(defmacro with-graphics-state (&body body)
  `(progn
     (save-graphics-state)
     (progn ,@body)
     (restore-graphics-state)))

(define-op set-canvas (xmin number) (ymin number) (xmax number) (ymax number))

(defmacro with-box-canvas (box &body code)
  (declare (ignore box))
  `(with-program
     ,@code))

(defun save-png (file)
  (declare (ignore file
                   )))

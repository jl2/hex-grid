;; hex-coordinates.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :hex-grid)

(defstruct (cube-coordinate
            (:constructor cube)
            (:conc-name cube-))
  "A cube coordinate for a cell in a hexagon grid."
  (q 0 :type fixnum)
  (r 0 :type fixnum)
  (s 0 :type fixnum))

(defstruct (axial-coordinate
            (:constructor axial)
            (:conc-name axial-))
  "An axial coordinate for a cell in a hexagon grid."
  (q 0 :type fixnum)
  (r 0 :type fixnum))

(defstruct (oddq-coordinate
            (:constructor oddq)
            (:conc-name oddq-))
  "An oddq coordinate for a cell in a hexagon grid."
  (col 0 :type fixnum)
  (row 0 :type fixnum))

(defstruct (evenq-coordinate
            (:constructor evenq)
            (:conc-name evenq-))
  "An evenq coordinate for a cell in a hexagon grid."
  (col 0 :type fixnum)
  (row 0 :type fixnum))

(defstruct (oddr-coordinate
            (:constructor oddr)
            (:conc-name oddr-))
  "An oddr coordinate for a cell in a hexagon grid."
  (col 0 :type fixnum)
  (row 0 :type fixnum))

(defstruct (evenr-coordinate
            (:constructor evenr)
            (:conc-name evenr-))
  "An evenr coordinate for a cell in a hexagon grid."
  (col 0 :type fixnum)
  (row 0 :type fixnum))


(defun cube-coord-sums-to-zero (cube)
  (with-slots (q r s) cube
    (zerop (+ q r s))))

(deftype valid-cube-coordinate ()
  "A cube-coordinate where the components sum to zero."
  '(and
    cube-coordinate
    (satisfies cube-coord-sums-to-zero)))

(deftype hex-coordinate ()
  "Any type of valid hex coordinate."
  '(or
    axial-coordinate
    valid-cube-coordinate
    evenr-coordinate oddr-coordinate
    evenq-coordinate oddq-coordinate))


(declaim (ftype (function (hex-coordinate hex-coordinate) cube-coordinate)
                cube-add cube-sub))
(defun cube-add (a b)
  "Add two hex-coordinates.  Converts both parameters to cube-coordinate and returns an cube-coordinate."
  (declare (type hex-coordinate a b))
  (let ((aa (to-cube a))
        (bb (to-cube b)))
    (cube :q (the fixnum
                  (+ (cube-q aa)
                     (cube-q bb)))
          :r (the fixnum
                  (+ (cube-r aa)
                     (cube-r bb)))
          :s (the fixnum
                  (+ (cube-s aa)
                     (cube-s bb))))))

(defun cube-sub (a b)
  "Add two hex-coordinates.  Converts both parameters to cube-coordinate and returns an cube-coordinate."
  (declare (type hex-coordinate a b))
  (let ((aa (to-cube a))
        (bb (to-cube b)))
    (cube :q (the fixnum
                  (- (cube-q aa)
                     (cube-q bb)))
          :r (the fixnum
                  (- (cube-r aa)
                     (cube-r bb)))
          :s (the fixnum
                  (- (cube-s aa)
                     (cube-s bb))))))

(declaim (ftype (function (hex-coordinate fixnum) cube-coordinate)
                cube-scale))
(defun cube-scale (hex factor)
  "Convert to hex cube coordinates and scale by factor."
  (declare (type hex-coordinate hex)
           (type fixnum factor))
  (let ((cbe (to-cube hex)))
    (cube :q (the fixnum
                  (* (cube-q cbe)
                     factor))
          :r (the fixnum
                  (* (cube-r cbe)
                     factor))
          :s (the fixnum
                  (* (cube-s cbe)
                     factor)))))

(declaim (ftype (function (hex-coordinate hex-coordinate) fixnum)
                hex-distance))
(defun hex-distance (a b)
  "Find the cube coordinate distance between a and b.  Converts both parameters to cube-coordinate and returns a fixnum."
  (declare (type hex-coordinate a b))
  (let ((diff (cube-sub a b)))
    (the fixnum
         (max (abs (cube-q diff))
              (abs (cube-r diff))
              (abs (cube-s diff))))))

(declaim (ftype (function (hex-coordinate fixnum) list)
                hex-ring))
(defun hex-ring (center radius)
  "Find all hexagons radius units away from center."
  (declare (type hex-coordinate center)
           (type fixnum radius))
  (let ((hex (cube-add center (cube-scale (aref *cube-direction-vectors* 4) radius)))
        (results nil))
    (loop
      :for i :below 6
      :do
         (loop
           :for j :below radius
           :do 
              (push hex results)
              (setf hex (cube-neighbor hex i))))
    results))

(declaim (ftype (function (hex-coordinate hex-coordinate) axial-coordinate)
                axial-add axial-sub))
(defun axial-add (a b)
  "Add two hex-coordinates.  Converts both parameters to axial-coordinate and returns an axial-coordinate."
  (declare (type hex-coordinate a b))
  (let ((aa (to-axial a))
        (bb (to-axial b)))
  (axial :q (the fixnum
                 (+ (axial-q aa)
                    (axial-q bb)))
         :r (the fixnum
                 (+ (axial-r aa)
                    (axial-r bb))))))

(declaim (ftype (function (hex-coordinate hex-coordinate) fixnum)
                axial-distance))

(defun axial-sub (a b)
  "Add two hex-coordinates.  Converts both parameters to axial-coordinate and returns an axial-coordinate."
  (declare (type hex-coordinate a b))
  (let ((aa (to-axial a))
        (bb (to-axial b)))
  (axial :q (the fixnum
                 (- (axial-q aa)
                    (axial-q bb)))
         :r (the fixnum
                 (- (axial-r aa)
                    (axial-r bb))))))

(declaim (ftype (function (hex-coordinate hex-coordinate) oddr-coordinate)
                oddr-add oddr-sub))
(defun oddr-add (a b)
  "Add two hex-coordinates.  Converts both parameters to axial-coordinate and returns an axial-coordinate."
  (declare (type hex-coordinate a b))
  (let ((aa (to-oddr a))
        (bb (to-oddr b)))
    (oddr :col (the fixnum
                    (+ (oddr-col aa)
                       (oddr-col bb)))
          :row (the fixnum
                    (+ (oddr-row aa)
                       (oddr-row bb))))))

(defun oddr-sub (a b)
  "Add two hex-coordinates.  Converts both parameters to axial-coordinate and returns an axial-coordinate."
  (declare (type hex-coordinate a b))
  (let ((aa (to-oddr a))
        (bb (to-oddr b)))
    (oddr :col (the fixnum
                    (- (oddr-col aa)
                       (oddr-col bb)))
          :row (the fixnum
                    (- (oddr-row aa)
                       (oddr-row bb))))))

(declaim (ftype (function (fixnum) (integer 0 1)) is-even))
(defun is-even (num)
  "Return 0 if num is even, 1 if odd."
  (declare (fixnum num))
  (the fixnum
       (logand num 1)))

(defparameter *axial-direction-vectors*
  (make-array 6 :element-type 'axial-coordinate
                :initial-contents
                `(,(axial :q 1 :r 0)
                  ,(axial :q 1 :r -1)
                  ,(axial :q 0 :r -1)
                  ,(axial :q -1 :r 0)
                  ,(axial :q -1 :r 1)
                  ,(axial :q 0 :r 1)))
  "The coordinate offsets for all neighbors of a hexagon in axial coordinates.")

(defparameter *cube-direction-vectors*
  (make-array 6 :element-type 'cube-coordinate
                :initial-contents
                `(,(cube :q 1 :r 0 :s -1)
                  ,(cube :q 1 :r -1 :s 0)
                  ,(cube :q 0 :r -1 :s 1)
                  ,(cube :q -1 :r 0 :s 1)
                  ,(cube :q -1 :r 1 :s 0)
                  ,(cube :q 0 :r 1 :s -1)))
  "The coordinate offsets for all neighbors of a hexagon in cube coordinates.")

(defun axial-neighbor (coord i)
  "Return the coordinate of a single hexagon neighboring the coord."
  (declare (type hex-coordinate coord)
           (type (integer 0 6) i))
  (axial-add (to-axial coord) (aref *axial-direction-vectors* i)))

(defun cube-neighbor (coord i)
  "Return the coordinate of a single hexagon neighboring the coord."
  (declare (type hex-coordinate coord)
           (type (integer 0 6) i))
  (cube-add (to-cube coord) (aref *cube-direction-vectors* i)))

(defun neighbors (coord)
  "Return the coordinates of all 6 of a hexagon's neighbors."
  (declare (type hex-coordinate coord))
  (map 'vector
       (lambda (val) (axial-add (to-axial coord) val))
       *axial-offsets*))

(defun center (coord &optional (hex-radius 1.0) (hex-type :flat))
  "Return the center of the hexagon at coord."
  (declare (type hex-coordinate coord))
  (with-slots (q r) (to-axial coord)
    (if (eq :flat hex-type)
        (vec2 (* hex-radius (/ 3 2) q)
              (* hex-radius (+ (* (/ (sqrt 3) 2) q)
                               (* (sqrt 3) r))))

        ;; Pointy
        (vec2 (* hex-radius (+ (* (sqrt 3) q)
                               (* (/ (sqrt 3) 2) r)))
              (* hex-radius
                 (/ 3
                    2)
                 r)))))

(defun hex-vert (center radius n &optional (offset-angle 0.0d0))
  "Return the coordinate o of a hexagon centered at center."
  (declare (type vec2 center)
           (type real radius offset-angle)
           (type (integer 0 6) n))
  (let ((theta (+ offset-angle (/ (* n 2 pi) 6))))
    (v+ center
        (vec2 (* radius (cos theta))
              (* radius (sin theta))))))

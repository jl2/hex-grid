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

(defgeneric hex-add (a b))

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

(defparameter *axial-offsets*
  (make-array 6 :element-type 'axial-coordinate
                :initial-contents
                `(,(axial :q 1 :r 0)
                  ,(axial :q 1 :r -1)
                  ,(axial :q 0 :r -1)
                  ,(axial :q -1 :r 0)
                  ,(axial :q -1 :r 1)
                  ,(axial :q 0 :r 1)))
  "The coordinate offsets for all neighbors of a hexagon.")

(defun neighbor (coord i)
  "Return the coordinate of a single hexagon neighboring the coord."
  (declare (type hex-coordinate coord)
           (type (integer 0 6) i))
  (axial-add (to-axial coord) (aref *axial-offsets* i)))

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

(defun hex-vert (center radius n &optional (offset-angle 0.0))
  "Return the coordinate o of a hexagon centered at center."
  (declare (type vec2 center)
           (type double-float radius offset-angle)
           (type (integer 0 6) n))
  (let ((theta (+ offset-angle (/ (* n 2 pi) 6))))
    (v+ center
        (vec2 (* radius (cos theta))
              (* radius (sin theta))))))

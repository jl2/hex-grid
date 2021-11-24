;; hex-grid.lisp
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

(defclass hex-grid ()
  ((hex-radius :initform 1.0f0 :initarg :hex-radius)
   (radius :initform 10 :initarg :radius :type fixnum)
   (hex-type :initform :flat :initarg :hex-type)
   (hex-count :initform 0)
   (default-state :initform 0 :initarg :default-state)
   (center-address :initform (axial :q 0 :r 0) :initarg :center-address)
   (center-coord :initform (vec2 0.0 0.0) :initarg :center-coord)))

(defgeneric state (hg coordinate)
  (:documentation "Return the state of cell qr."))

(defmethod state ((hg hex-grid) addr)
  (declare (ignorable addr))
  (slot-value hg 'default-state))

(defparameter *axial-offsets*
  (make-array 6 :element-type 'axial-coordinate
                :initial-contents
                `(,(axial :q 1 :q 0)
                  ,(axial :q 1 :q -1)
                  ,(axial :q 0 :q -1)
                  ,(axial :q -1 :q 0)
                  ,(axial :q -1 :q 1)
                  ,(axial :q 0 :q 1))))

(defun neighbor (address i)
  (declare (type axial-coordinate address)
           (type (integer 0 6) i))
  (axial-add address (aref *axial-offsets* i)))

(defun neighbors (address)
  (declare (type axial-coordinate address))
    (map 'array (curry #'axial-add address) *axial-offsets*))

(defun center (address &optional (hex-radius 1.0) (hex-type :flat))
  (declare (type axial-coordinate address))
  (let ((q (axial-q address))
        (r (axial-r address)))

    (if (eq :flat hex-type)

        (vec2 (* hex-radius (/ 3 2) q)
              (* hex-radius (+ (* (/ (sqrt 3) 2) q)
                               (* (sqrt 3) r))))

        ;; Pointy
        (vec2 (* hex-radius (+ (* (sqrt 3) q)
                               (* (/ (sqrt 3) 2) r)))
              (* hex-radius (/ 3 2) r)))))

(defun hex-vert (center radius angle num)
  (let ((this-theta (+ angle
                       (/ (* num 2 pi) 6))))
    (v+ center
        (vec2 (* radius (cos this-theta))
              (* radius (sin this-theta))))))

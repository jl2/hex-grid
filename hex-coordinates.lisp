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

(defun sums-to-zero (a)
  (zerop (+ (aref a 0) (aref a 1) (aref a 2))))

(defstruct (cube-coordinate
            (:constructor cube)
            (:conc-name cube-))
  (q 0 :type fixnum)
  (r 0 :type fixnum)
  (s 0 :type fixnum))

(defstruct (axial-coordinate
            (:constructor axial)
            (:conc-name axial-))
  (q 0 :type fixnum)
  (r 0 :type fixnum))

(defstruct (oddq-coordinate
            (:constructor oddq)
            (:conc-name oddq-))
  (col 0 :type fixnum)
  (row 0 :type fixnum))

(defstruct (evenq-coordinate
            (:constructor evenq)
            (:conc-name evenq-))
  (col 0 :type fixnum)
  (row 0 :type fixnum))

(defstruct (oddr-coordinate
            (:constructor oddr)
            (:conc-name oddr-))
  (col 0 :type fixnum)
  (row 0 :type fixnum))

(defstruct (evenr-coordinate
            (:constructor evenr)
            (:conc-name evenr-))
  (col 0 :type fixnum)
  (row 0 :type fixnum))

(declaim (ftype (function (axial-coordinate axial-coordinate) axial-coordinate)
                axial-add))
(defun axial-add (a b)
  (declare (type axial-coordinate a b))
  (axial :q (the fixnum (+ (axial-q a)
                           (axial-q b)))
         :r (the fixnum (+ (axial-r a)
                           (axial-r b)))))

(declaim (ftype (function ((or oddq-coordinate evenq-coordinate
                               oddr-coordinate evenr-coordinate
                               cube-coordinate axial-coordinate))
                          (values axial-coordinate &optional))
                to-axial))
(declaim (ftype (function ((or  oddq-coordinate evenq-coordinate
                                oddr-coordinate evenr-coordinate
                                cube-coordinate axial-coordinate))
                          (values cube-coordinate &optional))
                to-cube))
(declaim (ftype (function ((or oddq-coordinate evenq-coordinate
                               oddr-coordinate evenr-coordinate
                               cube-coordinate axial-coordinate))
                          (values oddq-coordinate &optional))
                to-oddq))
(declaim (ftype (function ((or oddq-coordinate evenq-coordinate
                               oddr-coordinate evenr-coordinate
                               cube-coordinate axial-coordinate))
                          (values evenq-coordinate &optional))
                to-evenq))
(declaim (ftype (function ((or oddq-coordinate evenq-coordinate
                               oddr-coordinate evenr-coordinate
                               cube-coordinate axial-coordinate))
                          (values oddr-coordinate &optional))
                to-oddr))
(declaim (ftype (function ((or oddq-coordinate evenq-coordinate
                               oddr-coordinate evenr-coordinate
                               cube-coordinate axial-coordinate))
                          (values evenr-coordinate &optional))
                to-evenr))


(declaim (ftype (function (fixnum) (integer 0 1)) is-even))
(defun is-even (num)
  (declare (fixnum num))
  (the fixnum (logand num 1)))

(declaim (inline to-axial to-cube to-evenr to-evenq to-oddr to-oddq))
(defgeneric to-axial (coord))
(defgeneric to-cube (coord))
(defgeneric to-evenr (coord))
(defgeneric to-evenq (coord))
(defgeneric to-oddr (coord))
(defgeneric to-oddq (coord))


(defmethod to-axial ((coord cube-coordinate))
  (axial :q (cube-q coord)
         :r (cube-r coord)))

(defmethod to-axial ((coord oddr-coordinate))
  (axial :q (- (oddr-col coord)
               (/ (- (oddr-row coord)
                     (is-even (oddr-row coord))) 2))
         :r (oddr-row coord)))

(defmethod to-axial ((coord evenr-coordinate))
  (let* ((col (evenr-col coord))
         (row (evenr-row coord)))
    (axial :q (- col (/ (+ row (is-even row)) 2))
           :r row)))

(defmethod to-axial ((coord axial-coordinate))
  coord)

(defmethod to-cube ((coord axial-coordinate))
  (cube :q (axial-q coord)
        :r (axial-r coord)
        :s (- (- (axial-q coord))
              (axial-r coord))))
(defmethod to-cube ((coord oddr-coordinate))
  (let* ((row (oddr-row coord))
         (col (oddr-col coord))
         (q (- col (/ (- row (is-even row))
                      2))))
    (cube :q q
          :r row
          :s (- (- q) row))))
(defmethod to-cube ((coord evenr-coordinate))
  (let* ((row (oddr-row coord))
         (col (oddr-col coord))
         (q (- col (/ (+ row (is-even row))
                      2))))
    (cube :q q
          :r row
          :s (- (- q) row))))
(defmethod to-cube ((coord cube-coordinate))
  coord)

(defmethod to-oddr ((coord axial-coordinate))
  (let ((q (axial-q coord))
        (r (axial-r coord)))
    (oddr :col (+ q (/ (- r (is-even r))
                       2))
          :row r)))
(defmethod to-oddr ((coord cube-coordinate))
  (let ((q (cube-q coord))
        (r (cube-r coord)))
    (oddr :col (+ q (/ (- r (is-even r))
                       2))
          :row r)))
(defmethod to-oddr ((coord oddr-coordinate))
  coord)

(defmethod to-evenr ((coord axial-coordinate))
  (let ((q (axial-q coord))
        (r (axial-r coord)))
    (evenr :col (+ q (/ (+ r (is-even r))
                        2))
           :row r)))

(defmethod to-evenr ((coord cube-coordinate))
     (let ((q (axial-q coord))
           (r (axial-r coord)))
       (evenr :col (+ q (/ (+ r (is-even r))
                           2))
              :row r)))
(defmethod to-evenr ((coord evenr-coordinate))
  coord)

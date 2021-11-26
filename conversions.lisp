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

(declaim (inline to-axial to-cube to-evenr to-evenq to-oddr to-oddq))

(defgeneric to-axial (coord)
  (:documentation "Convert coord to an axial-coordinate."))
(defgeneric to-cube (coord)
  (:documentation "Convert coord to a cube-coordinate."))

(defgeneric to-evenr (coord)
  (:documentation "Convert coord to an evenr-coordinate."))
(defgeneric to-oddr (coord)
  (:documentation "Convert coord to an oddr-coordinate."))

(defgeneric to-evenq (coord)
  (:documentation "Convert coord to an evenq-coordinate."))
(defgeneric to-oddq (coord)
  (:documentation "Convert coord to an oddq-coordinate."))


(defmethod to-axial ((coord cube-coordinate))
  (axial :q (cube-q coord)
         :r (cube-r coord)))

(defmethod to-axial ((coord oddr-coordinate))
  (with-slots (col row) coord
    (axial :q (- col (/ (- row (is-even row))
                        2))
           :r row)))

(defmethod to-axial ((coord evenr-coordinate))
  (with-slots (col row) coord
    (axial :q (- col (/ (+ row (is-even row))
                        2))
           :r row)))

(defmethod to-axial ((coord axial-coordinate))
  coord)

(defmethod to-cube ((coord axial-coordinate))
  (cube :q (axial-q coord)
        :r (axial-r coord)
        :s (- (- (axial-q coord))
              (axial-r coord))))

(defmethod to-cube ((coord oddr-coordinate))
  (with-slots (row col) coord
    (let ((q (- col (/ (- row (is-even row))
                       2))))
      (cube :q q
            :r row
            :s (- (- q) row)))))

(defmethod to-cube ((coord evenr-coordinate))
  (with-slots (row col) coord
    (let ((q (- col (/ (+ row (is-even row))
                       2))))
      (cube :q q
            :r row
            :s (- (- q) row)))))

(defmethod to-cube ((coord cube-coordinate))
  coord)

(defmethod to-oddr ((coord axial-coordinate))
  (with-slots (q r) coord
    (oddr :col (+ q (/ (- r (is-even r))
                       2))
          :row r)))
(defmethod to-oddr ((coord cube-coordinate))
  (with-slots (q r) coord
    (oddr :col (+ q (/ (- r (is-even r))
                       2))
          :row r)))
(defmethod to-oddr ((coord oddr-coordinate))
  coord)

(defmethod to-evenr ((coord axial-coordinate))
  (with-slots (q r) coord
    (evenr :col (+ q (/ (+ r (is-even r))
                        2))
           :row r)))

(defmethod to-evenr ((coord cube-coordinate))
  (with-slots (q r) coord
    (evenr :col (+ q (/ (+ r (is-even r))
                        2))
           :row r)))

(defmethod to-evenr ((coord evenr-coordinate))
  coord)

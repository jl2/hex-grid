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
  ((hex-radius :initform 1.0f0
               :initarg :hex-radius)
   (hex-type :initform :flat
             :initarg :hex-type)

   (default-state :initform 0
                  :initarg :default-state)

   (min-hex :initform (oddr :col 0 :row 0)
            :initarg :min-hex
            :accessor min-hex)
   (max-hex :initform (oddr :col 10 :row 10)
            :initarg :max-hex
            :accessor max-hex)

   (width :initform (oddr :col 10 :row 10)
          :initarg :width
          :accessor width)
   (state-idx :initform 0)
   (states :initarg :states
           :accessor states)))

(defun make-hex-grid (&key
                        (min-hex (oddr :col 0 :row 0))
                        (max-hex (oddr :col 10 :row 10))
                        (hex-type :flat)
                        (hex-radius 1.0)
                        (default-state 0))
  (with-slots ((min-col col) (min-row row)) (to-oddr min-hex)
    (with-slots ((max-col col) (max-row row)) (to-oddr max-hex)
      (let ((col-width (- max-col min-col))
            (row-width (- max-row min-row)))
      (make-instance 'hex-grid
                     :min-hex min-hex
                     :max-hex max-hex
                     :hex-type hex-type
                     :hex-radius hex-radius
                     :default-state default-state
                     :width (oddr :col col-width
                                  :row row-width)
                     :states (make-array (list row-width
                                               col-width)
                                         :element-type 'fixnum
                                         :initial-element default-state))))))

(defun state (hg coordinate)
  (with-slots (min-hex states width default-state) hg
    (with-slots (row col) (oddr-sub (to-oddr coordinate) min-hex)
      (with-slots ((max-row row) (max-col col)) width
        (cond
          ((and (< row max-row)
                (< col max-col)
                (>= col 0)
                (>= row 0))
           (aref states row col))
          (t
           ;; (format t "Returning default for ~a~%" coordinate)
           (random 3)))))))

(defun (setf state) (value hg coordinate)
  (with-slots (min-hex states width) hg
    (with-slots (row col) (oddr-sub (to-oddr coordinate) min-hex)
      (with-slots ((max-row row) (max-col col)) width
        (cond
          ((and (< row max-row)
                (>= row 0)
                (< col max-col)
                (>= col 0))
           (setf (aref states row col) value))
          (t
           (error "~a is not a valid coordinate for hex-grid ~a"
                  coordinate hg)))))))

(defun min-col (hg)
  (with-slots (min-hex) hg
    (with-slots (col) min-hex
      col)))

(defun min-row (hg)
  (with-slots (min-hex) hg
    (with-slots (row) min-hex
      row)))


(defun max-col (hg)
  (with-slots (max-hex) hg
    (with-slots (col) max-hex
      col)))

(defun max-row (hg)
  (with-slots (max-hex) hg
    (with-slots (row) max-hex
      row)))

(defun col-size (hg)
  (with-slots (width) hg
    (with-slots (col) width
      col)))

(defun row-size (hg)
  (with-slots (width) hg
    (with-slots (row) width
      row)))

(defun hex-count (hg)
  (* (col-size hg) (row-size hg)))

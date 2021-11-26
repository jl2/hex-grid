;; coordinates.lisp
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

(in-package :hex-grid.test)

(def-suite :hex-grid-coordinates)
(in-suite :hex-grid-coordinates)

(test cube-to-oddr
  (let ((oddrs
          (list (hg:oddr :col 0  :row 0)
                (hg:oddr :col 1  :row 0)
                (hg:oddr :col 1  :row -1)
                (hg:oddr :col -2 :row -2)
                (hg:oddr :col 2  :row 2)
                (hg:oddr :col -2 :row -1)
                (hg:oddr :col -1 :row -1)
                (hg:oddr :col -1 :row -2)))
        (cubes
          (list (hg:cube :Q 0 :R 0 :S 0)
                (hg:cube :Q 1 :R 0 :S -1)
                (hg:cube :Q 2 :R -1 :S -1)
                (hg:cube :Q -1 :R -2 :S 3)
                (hg:cube :Q 1 :R 2 :S -3)
                (hg:cube :Q -1 :R -1 :S 2)
                (hg:cube :Q 0 :R -1 :S 1)
                (hg:cube :Q 0 :R -2 :S 2))))
    (loop for oddr in oddrs
          for cube in cubes
          do
             (is (equalp (hg:to-oddr cube) oddr))
             (is (equalp (hg:to-cube oddr) cube)))))

(test cube-to-evenr
  (let ((evenrs
          (list (hg:evenr :col 0  :row 0)
                (hg:evenr :col 1  :row 0)
                (hg:evenr :col 1  :row -1)
                (hg:evenr :col -2 :row -2)
                (hg:evenr :col 2  :row 2)
                (hg:evenr :col -2 :row -1)
                (hg:evenr :col -1 :row -1)
                (hg:evenr :col -1 :row -2)))
        (cubes
          (list (hg:cube :Q 0 :R 0 :S 0)
                (hg:cube :Q 1 :R 0 :S -1)
                (hg:cube :Q 1 :R -1 :S 0)
                (hg:cube :Q -1 :R -2 :S 3)
                (hg:cube :Q 1 :R 2 :S -3)
                (hg:cube :Q -2 :R -1 :S 3)
                (hg:cube :Q -1 :R -1 :S 2)
                (hg:cube :Q 0 :R -2 :S 2))))
    (loop for evenr in evenrs
          for cube in cubes
          do
             (is (equalp (hg:to-evenr cube) evenr))
             (is (equalp (hg:to-cube evenr) cube)))))

(test axial-to-evenr
  (let ((evenrs
          (list (hg:evenr :col 0  :row 0)
                (hg:evenr :col 1  :row 0)
                (hg:evenr :col 1  :row -1)
                (hg:evenr :col -2 :row -2)
                (hg:evenr :col 2  :row 2)
                (hg:evenr :col -2 :row -1)
                (hg:evenr :col -1 :row -1)
                (hg:evenr :col -1 :row -2)))
        (axials
          (list (hg:axial :Q 0 :R 0)
                (hg:axial :Q 1 :R 0)
                (hg:axial :Q 1 :R -1)
                (hg:axial :Q -1 :R -2)
                (hg:axial :Q 1 :R 2)
                (hg:axial :Q -2 :R -1)
                (hg:axial :Q -1 :R -1)
                (hg:axial :Q 0 :R -2))))
    (loop for evenr in evenrs
          for axial in axials
          do
             (is (equalp (hg:to-evenr axial) evenr))
             (is (equalp (hg:to-axial evenr) axial)))))

(test axial-to-oddr
  (let ((oddrs
          (list (hg:oddr :col 0  :row 0)
                (hg:oddr :col 1  :row 0)
                (hg:oddr :col 1  :row -1)
                (hg:oddr :col -2 :row -2)
                (hg:oddr :col 2  :row 2)
                (hg:oddr :col -2 :row -1)
                (hg:oddr :col -1 :row -1)
                (hg:oddr :col -1 :row -2)))
        (axials
          (list (hg:axial :Q 0 :R 0)
                (hg:axial :Q 1 :R 0)
                (hg:axial :Q 2 :R -1)
                (hg:axial :Q -1 :R -2)
                (hg:axial :Q 1 :R 2)
                (hg:axial :Q -1 :R -1)
                (hg:axial :Q 0 :R -1)
                (hg:axial :Q 0 :R -2))))
    (loop for oddr in oddrs
          for axial in axials
          do
             (is (equalp (hg:to-oddr axial) oddr) )
             (is (equalp (hg:to-axial oddr) axial)))))

(test cube-to-axial
  (let ((axials
          (list (hg:axial :Q 0 :R 0)
                (hg:axial :Q 1 :R 0)
                (hg:axial :Q 1 :R -1)
                (hg:axial :Q -1 :R -2)
                (hg:axial :Q 1 :R 2)
                (hg:axial :Q -2 :R -1)
                (hg:axial :Q -1 :R -1)
                (hg:axial :Q 0 :R -2)))
        (cubes
          (list (hg:cube :Q 0 :R 0 :S 0)
                (hg:cube :Q 1 :R 0 :S -1)
                (hg:cube :Q 1 :R -1 :S 0)
                (hg:cube :Q -1 :R -2 :S 3)
                (hg:cube :Q 1 :R 2 :S -3)
                (hg:cube :Q -2 :R -1 :S 3)
                (hg:cube :Q -1 :R -1 :S 2)
                (hg:cube :Q 0 :R -2 :S 2))))
    (loop for axial in axials
          for cube in cubes
          do
             (is (equalp (hg:to-axial cube) axial))
             (is (equalp (hg:to-cube axial) cube)))))


;; (test cube-to-oddq
;;   (let ((oddqs
;;           (list (hg:oddq :col 0  :row 0)
;;                 (hg:oddq :col 1  :row 0)
;;                 (hg:oddq :col 1  :row -1)
;;                 (hg:oddq :col -2 :row -2)
;;                 (hg:oddq :col 2  :row 2)
;;                 (hg:oddq :col -2 :row -1)
;;                 (hg:oddq :col -1 :row -1)
;;                 (hg:oddq :col -1 :row -2)))
;;         (cubes
;;           (list (hg:cube :Q 0 :R 0 :S 0)
;;                 (hg:cube :Q 1 :R 0 :S -1)
;;                 (hg:cube :Q 2 :R -1 :S -1)
;;                 (hg:cube :Q -1 :R -2 :S 3)
;;                 (hg:cube :Q 1 :R 2 :S -3)
;;                 (hg:cube :Q -1 :R -1 :S 2)
;;                 (hg:cube :Q 0 :R -1 :S 1)
;;                 (hg:cube :Q 0 :R -2 :S 2))))
;;     (loop for oddq in oddqs
;;           for cube in cubes
;;           do
;;              (is (equalp (hg:to-oddq cube) oddq))
;;              (is (equalp (hg:to-cube oddq) cube)))))

;; (test cube-to-evenq
;;   (let ((evenqs
;;           (list (hg:evenq :col 0  :row 0)
;;                 (hg:evenq :col 1  :row 0)
;;                 (hg:evenq :col 1  :row -1)
;;                 (hg:evenq :col -2 :row -2)
;;                 (hg:evenq :col 2  :row 2)
;;                 (hg:evenq :col -2 :row -1)
;;                 (hg:evenq :col -1 :row -1)
;;                 (hg:evenq :col -1 :row -2)))
;;         (cubes
;;           (list (hg:cube :Q 0 :R 0 :S 0)
;;                 (hg:cube :Q 1 :R 0 :S -1)
;;                 (hg:cube :Q 1 :R -1 :S 0)
;;                 (hg:cube :Q -1 :R -2 :S 3)
;;                 (hg:cube :Q 1 :R 2 :S -3)
;;                 (hg:cube :Q -2 :R -1 :S 3)
;;                 (hg:cube :Q -1 :R -1 :S 2)
;;                 (hg:cube :Q 0 :R -2 :S 2))))
;;     (loop for evenq in evenqs
;;           for cube in cubes
;;           do
;;              (is (equalp (hg:to-evenq cube) evenq))
;;              (is (equalp (hg:to-cube evenq) cube)))))

;; (test axial-to-evenq
;;   (let ((evenqs
;;           (list (hg:evenq :col 0  :row 0)
;;                 (hg:evenq :col 1  :row 0)
;;                 (hg:evenq :col 1  :row -1)
;;                 (hg:evenq :col -2 :row -2)
;;                 (hg:evenq :col 2  :row 2)
;;                 (hg:evenq :col -2 :row -1)
;;                 (hg:evenq :col -1 :row -1)
;;                 (hg:evenq :col -1 :row -2)))
;;         (axials
;;           (list (hg:axial :Q 0 :R 0)
;;                 (hg:axial :Q 1 :R 0)
;;                 (hg:axial :Q 1 :R -1)
;;                 (hg:axial :Q -1 :R -2)
;;                 (hg:axial :Q 1 :R 2)
;;                 (hg:axial :Q -2 :R -1)
;;                 (hg:axial :Q -1 :R -1)
;;                 (hg:axial :Q 0 :R -2))))
;;     (loop for evenq in evenqs
;;           for axial in axials
;;           do
;;              (is (equalp (hg:to-evenq axial) evenq))
;;              (is (equalp (hg:to-axial evenq) axial)))))

;; (test axial-to-oddq
;;   (let ((oddqs
;;           (list (hg:oddq :col 0  :row 0)
;;                 (hg:oddq :col 1  :row 0)
;;                 (hg:oddq :col 1  :row -1)
;;                 (hg:oddq :col -2 :row -2)
;;                 (hg:oddq :col 2  :row 2)
;;                 (hg:oddq :col -2 :row -1)
;;                 (hg:oddq :col -1 :row -1)
;;                 (hg:oddq :col -1 :row -2)))
;;         (axials
;;           (list (hg:axial :Q 0 :R 0)
;;                 (hg:axial :Q 1 :R 0)
;;                 (hg:axial :Q 2 :R -1)
;;                 (hg:axial :Q -1 :R -2)
;;                 (hg:axial :Q 1 :R 2)
;;                 (hg:axial :Q -1 :R -1)
;;                 (hg:axial :Q 0 :R -1)
;;                 (hg:axial :Q 0 :R -2))))
;;     (loop for oddq in oddqs
;;           for axial in axials
;;           do
;;              (is (equalp (hg:to-oddq axial) oddq) )
;;              (is (equalp (hg:to-axial oddq) axial)))))

(test axial-add
  (let ((res (hg:axial-add (hg:axial :q 3 :r 4)
                           (hg:axial :q 6 :r 6))))
    (is (= (hg:axial-q res) 9))
    (is (= (hg:axial-r res) 10))))

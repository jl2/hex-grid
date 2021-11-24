;; package.lisp
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

(defpackage :hex-grid

  (:nicknames :hg)

  (:use #:cl #:j-utils #:alexandria #:3d-vectors)
  (:export #:hex-grid
           #:state

           #:neighbor
           #:neighbors
           #:axial-add
           #:radius
           #:hex-count
           #:center
           #:hex-vert

           #:axial-coordinate
           #:cube-coordinate
           #:oddq-coordinate
           #:evenq-coordinate
           #:oddr-coordinate
           #:evenr-coordinate

           #:axial
           #:cube
           #:oddq
           #:evenq
           #:oddr
           #:evenr

           #:to-cube
           #:to-axial
           #:to-oddq
           #:to-evenq
           #:to-oddr
           #:to-evenr

           #:axial-q
           #:axial-r

           #:cube-q
           #:cube-r
           #:cube-s

           #:oddq-col
           #:oddq-row
           #:evenq-col
           #:evenq-row

           #:oddr-col
           #:oddr-row
           #:evenr-col
           #:evenr-row

           #:col
           #:row
           ))

;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of guile-sdl2.
;;;
;;; Guile-sdl2 is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-sdl2 is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with guile-sdl2.  If not, see
;;; <http://www.gnu.org/licenses/>.

(use-modules (sdl2)
             (sdl2 render)
             (sdl2 surface)
             (sdl2 video))

(define (draw ren)
  (let* ((surface (load-bmp "hello.bmp"))
         (texture (surface->texture ren surface)))
    (clear-renderer ren)
    (render-copy ren texture)
    (present-renderer ren)
    (sleep 2)))

(sdl-init)

(call-with-window (make-window)
  (lambda (window)
    (call-with-renderer (make-renderer window) draw)))

(sdl-quit)

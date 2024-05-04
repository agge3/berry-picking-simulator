;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2017, 2021 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2022 Ekaitz Zarraga <ekaitz@elenq.tech>
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

;;; Commentary:
;;
;; Mouse input and cursor manipulation.
;;
;;; Code:

(define-module (sdl2 input mouse)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (system foreign)
  #:export (mouse-x
            mouse-y
            mouse-button-pressed?
            mouse-button-released?
            set-show-cursor!
            cursor-visible?
            warp-mouse
            make-system-cursor
            make-color-cursor
            get-cursor
            set-cursor!
            delete-cursor!))

(define (make-int)
  (make-bytevector (sizeof int)))

(define (get-int bv)
  (bytevector-sint-ref bv 0 (native-endianness) (sizeof int)))

(define mouse-x
  (let* ((bv (make-int))
         (ptr (bytevector->pointer bv)))
    (lambda ()
      "Return the x coordinate of the mouse cursor."
      (ffi:sdl-get-mouse-state ptr %null-pointer)
      (get-int bv))))

(define mouse-y
  (let* ((bv (make-int))
         (ptr (bytevector->pointer bv)))
    (lambda ()
      "Return the y coordinate of the mouse cursor."
      (ffi:sdl-get-mouse-state %null-pointer ptr)
      (get-int bv))))

(define (mouse-button-pressed? button)
  "Return #t if BUTTON is currently being pressed."
  (let ((mask (match button
                ('left ffi:SDL_BUTTON_LMASK)
                ('middle ffi:SDL_BUTTON_MMASK)
                ('right ffi:SDL_BUTTON_RMASK)
                ('x1 ffi:SDL_BUTTON_X1MASK)
                ('x2 ffi:SDL_BUTTON_X2MASK)))
        (buttons (ffi:sdl-get-mouse-state %null-pointer %null-pointer)))
    (> (logand mask buttons) 0)))

(define (mouse-button-released? button)
  "Return #t if BUTTON is not currently being pressed."
  (not (mouse-button-pressed? button)))

(define (set-show-cursor! show?)
  "If SHOW? is #t, show the mouse cursor.  Otherwise, hide it."
  (when (< (ffi:sdl-show-cursor (if show? ffi:SDL_ENABLE ffi:SDL_DISABLE)) 0)
    (sdl-error "set-show-cursor!" "failed to modify cursor visibility")))

(define (cursor-visible?)
  "Return #t if the mouse cursor is currently visible."
  (= (ffi:sdl-show-cursor ffi:SDL_QUERY) ffi:SDL_ENABLE))

(define* (warp-mouse x y #:optional window)
  "Warp mouse cursor to (X, Y) relative to WINDOW.  If WINDOW is not
provided, the mouse cursor is moved to the global screen
coordinates (X, Y)."
  (if window
      (ffi:sdl-warp-mouse-in-window ((@@ (sdl2 video) unwrap-window) window)
                                    x y)
      (unless (= (ffi:sdl-warp-mouse-global x y) 0)
        (sdl-error "warp-mouse" "failed to warp mouse globally"))))


;;;
;;; Cursors
;;;

(define-wrapped-pointer-type <cursor>
  cursor?
  wrap-cursor unwrap-cursor
  (lambda (cursor port)
    (format port "#<cursor ~x>"
            (pointer-address (unwrap-cursor cursor)))))

(define (make-system-cursor cursor-type)
  "Return a new cursor from the system's available set, chosen by
CURSOR-TYPE.  Valid cursor types are: arrow, crosshair, hand, i-beam,
no, size-north-south, size-northwest-southeast,
size-northeast-southwest, size-east-west, size-all, wait, and
wait-arrow."
  (let* ((id (case cursor-type
               ((arrow) ffi:SDL_SYSTEM_CURSOR_ARROW)
               ((crosshair) ffi:SDL_SYSTEM_CURSOR_CROSSHAIR)
               ((hand) ffi:SDL_SYSTEM_CURSOR_HAND)
               ((i-beam) ffi:SDL_SYSTEM_CURSOR_IBEAM)
               ((no) ffi:SDL_SYSTEM_CURSOR_NO)
               ((size-north-south) ffi:SDL_SYSTEM_CURSOR_SIZENS)
               ((size-northwest-southeast) ffi:SDL_SYSTEM_CURSOR_SIZENWSE)
               ((size-northeast-southwest) ffi:SDL_SYSTEM_CURSOR_SIZENESW)
               ((size-west-east) ffi:SDL_SYSTEM_CURSOR_SIZEWE)
               ((size-all) ffi:SDL_SYSTEM_CURSOR_SIZEALL)
               ((wait) ffi:SDL_SYSTEM_CURSOR_WAIT)
               ((wait-arrow) ffi:SDL_SYSTEM_CURSOR_WAITARROW)
               (else
                 (sdl-error "make-system-cursor" "unknown cursor type"))))
         (ptr (ffi:sdl-create-system-cursor id)))
    (if (null-pointer? ptr)
        (sdl-error "make-system-cursor" "failed to create system cursor")
        (wrap-cursor ptr))))

(define (make-color-cursor surface hot-x hot-y)
  "Make a cursor from SURFACE with a hot spot of (HOT-X, HOT-Y)."
  (let ((ptr (ffi:sdl-create-color-cursor
               ((@@ (sdl2 surface) unwrap-surface) surface) hot-x hot-y)))
    (if (null-pointer? ptr)
        (sdl-error "make-color-cursor" "failed to create color cursor")
        (wrap-cursor ptr))))

(define (get-cursor)
  "Return the cursor currently in use.  The returned cursor object is
internally managed and it's not necessary to call delete-cursor! for
it."
  (wrap-cursor (ffi:sdl-get-cursor)))

(define (set-cursor! cursor)
  "Set current cursor to CURSOR.  If CURSOR is #f, the system default
cursor is restored."
  (ffi:sdl-set-cursor (if cursor
                        (unwrap-cursor cursor)
                        %null-pointer)))

(define (delete-cursor! cursor)
  "Free the memory used by CURSOR.  Be careful!"
  (ffi:sdl-free-cursor (unwrap-cursor cursor)))

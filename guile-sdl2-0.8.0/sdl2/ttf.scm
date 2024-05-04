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

;;; Commentary:
;;
;; Font rendering.
;;
;;; Code:

(define-module (sdl2 ttf)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings ttf) #:prefix ffi:)
  #:export (ttf-init
            ttf-quit

            font?
            load-font
            delete-font!
            font-height
            font-ascent
            font-descent
            font-line-skip
            font-size
            font-glyph-index
            font-glyph-metrics
            font-style
            set-font-style!

            render-font-solid
            render-font-blended))

(define (ttf-init)
  "Initialize the TTF system."
  (unless (zero? (ffi:ttf-init))
    (sdl-error "ttf-init" "failed to initialize TTF library")))

(define (ttf-quit)
  "Shut down and clean up the TTF system."
  (ffi:ttf-quit))

(define-wrapped-pointer-type <font>
  font?
  wrap-font unwrap-font
  (lambda (font port)
    (format port "#<font ~x>"
            (pointer-address (unwrap-font font)))))

(define (load-font file point-size)
  "Load TTF font from FILE and return a new font object whose glyph
size is POINT-SIZE."
  (let ((ptr (ffi:ttf-open-font (string->pointer file) point-size)))
    (if (null-pointer? ptr)
        (sdl-error "load-font" "failed to load font" file)
        (wrap-font ptr))))

(define (delete-font! font)
  "Delete the memory allocated for FONT."
  (ffi:ttf-close-font (unwrap-font font)))

(define (font-height font)
  "Return the maximum height of FONT."
  (ffi:ttf-font-height (unwrap-font font)))

(define (font-ascent font)
  "Return the maximum pixel ascent of all glyphs in FONT."
  (ffi:ttf-font-ascent (unwrap-font font)))

(define (font-descent font)
  "Return the maximum pixel descent of all glyphs in FONT."
  (ffi:ttf-font-descent (unwrap-font font)))

(define (font-line-skip font)
  "Return the recommended pixel height of a line in FONT."
  (ffi:ttf-font-line-skip (unwrap-font font)))

(define (font-size font text)
  "Return 2 values for the resulting surface size of the string TEXT
using FONT."
  (let ((bv (make-s32vector 2)))
    (if (zero? (ffi:ttf-size-utf8 (unwrap-font font)
                                  (string->pointer text)
                                  (bytevector->pointer bv)
                                  (bytevector->pointer bv 4)))
        (values (s32vector-ref bv 0) (s32vector-ref bv 1))
        (sdl-error "size-utf8" "failed to get size utf8"))))

(define (font-glyph-index font char)
  "Return the index of the glyph for CHAR in FONT, or #f if CHAR is
not present."
  (let ((result (ffi:ttf-glyph-is-provided (unwrap-font font) (char->integer char))))
    (if (eq? result 0) #f result)))

(define (font-glyph-metrics font char)
  "Return 5 values for the metrics of CHAR in FONT: min x, max x, min y,
max y, and advance."
  (let ((bv (make-s32vector 5)))
    (if (zero? (ffi:ttf-glyph-metrics (unwrap-font font)
                                      (char->integer char)
                                      (bytevector->pointer bv)
                                      (bytevector->pointer bv 4)
                                      (bytevector->pointer bv 8)
                                      (bytevector->pointer bv 12)
                                      (bytevector->pointer bv 16)))
        (values (s32vector-ref bv 0)
                (s32vector-ref bv 1)
                (s32vector-ref bv 2)
                (s32vector-ref bv 3)
                (s32vector-ref bv 4))
        (sdl-error "font-glyph-metrics" "failed to get glyph metrics"))))

(define (font-style font)
  "Return the rendering style of FONT.  Return a list that may contain
any of the following symbols:

- bold
- italic
- underline
- strikethrough

The empty list returned if no there is no style applied."
  (let ((bitmask (ffi:ttf-get-font-style (unwrap-font font))))
    (filter-map (match-lambda
                  ((sym . bit)
                   (and (not (zero? (logand bitmask bit))) sym)))
                `((bold          . ,ffi:SDL_TTF_STYLE_BOLD)
                  (italic        . ,ffi:SDL_TTF_STYLE_ITALIC)
                  (underline     . ,ffi:SDL_TTF_STYLE_UNDERLINE)
                  (strikethrough . ,ffi:SDL_TTF_STYLE_STRIKETHROUGH)))))

(define (set-font-style! font style)
  "Set the rendering style of FONT to STYLE, a list that may contain
any of the following symbols:

- bold
- italic
- underline
- strikethrough

Use an empty list to set the normal style."
  (let ((bitmask
         (fold (lambda (flag prev)
                 (logior prev
                         (match flag
                           ('bold          ffi:SDL_TTF_STYLE_BOLD)
                           ('italic        ffi:SDL_TTF_STYLE_ITALIC)
                           ('underline     ffi:SDL_TTF_STYLE_UNDERLINE)
                           ('strikethrough ffi:SDL_TTF_STYLE_STRIKETHROUGH))))
               0
               style)))
    (ffi:ttf-set-font-style (unwrap-font font) bitmask)))

(define (render-font-solid font text color)
  "Render TEXT, a UTF-8 encoded string, using FONT and COLOR, the
foreground color, and return a surface containing the results."
  (let ((ptr (ffi:ttf-render-utf8-solid (unwrap-font font)
                                        (string->pointer text)
                                        ((@@ (sdl2) color->struct) color))))
    (if (null-pointer? ptr)
        (sdl-error "render-font-solid" "failed to render text")
        ((@@ (sdl2 surface) wrap-surface) ptr))))

(define (render-font-blended font text color)
  "Render TEXT, a UTF-8 encoded string, using FONT and COLOR, the
foreground color, and return a high-quality alpha-blended surface
containing the results."
  (let ((ptr (ffi:ttf-render-utf8-blended (unwrap-font font)
                                          (string->pointer text)
                                          ((@@ (sdl2) color->struct) color))))
    (if (null-pointer? ptr)
        (sdl-error "render-font-blended" "failed to render text")
        ((@@ (sdl2 surface) wrap-surface) ptr))))

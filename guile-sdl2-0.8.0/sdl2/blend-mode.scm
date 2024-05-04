;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2020 Leo Prikler <leo.prikler@student.tugraz.at>
;;; Copyright © 2020 David Thompson <davet@gnu.org>
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

(define-module (sdl2 blend-mode)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:export (make-blend-mode
            blend-mode?
            blend-mode-src-color-factor
            blend-mode-dst-color-factor
            blend-mode-color-operation
            blend-mode-src-alpha-factor
            blend-mode-dst-alpha-factor
            blend-mode-alpha-operation
            none
            blend
            add
            mod
            mul))

(define-record-type <blend-mode>
  (%make-blend-mode src-color-factor dst-color-factor color-operation
                    src-alpha-factor dst-alpha-factor alpha-operation
                    bitmask)
  blend-mode?
  (src-color-factor blend-mode-src-color-factor)
  (dst-color-factor blend-mode-dst-color-factor)
  (color-operation  blend-mode-color-operation)
  (src-alpha-factor blend-mode-src-alpha-factor)
  (dst-alpha-factor blend-mode-dst-alpha-factor)
  (alpha-operation  blend-mode-alpha-operation)
  (bitmask blend-mode-bitmask))

(define none
  (%make-blend-mode 'one 'zero 'add 'one 'zero 'add
                    ffi:SDL_BLENDMODE_NONE))

(define blend
  (%make-blend-mode 'src-alpha 'one-minus-src-alpha 'add
                    'one       'one-minus-src-alpha 'add
                    ffi:SDL_BLENDMODE_BLEND))

(define add
  (%make-blend-mode 'src-alpha 'one 'add
                    'zero      'one 'add
                    ffi:SDL_BLENDMODE_ADD))

(define mod
  (%make-blend-mode 'zero 'src-color 'add
                    'zero 'one       'add
                    ffi:SDL_BLENDMODE_MOD))

(define mul
  (%make-blend-mode 'dst-color 'one-minus-src-alpha 'add
                    'dst-alpha 'one-minus-src-alpha 'add
                    ffi:SDL_BLENDMODE_MUL))

(define (make-blend-mode src-color-factor dst-color-factor color-operation
                         src-alpha-factor dst-alpha-factor alpha-operation)
  "Return a new custom blend mode for renderers.

SRC-COLOR-FACTOR applies to the red, green, and blue components of the
source pixels.

DST-COLOR-FACTOR applies to the red, green, and blue components of the
destination pixels.

COLOR-OPERATION specifies how to combine the red, green, and blue
components of the source and destination pixels.

SRC-ALPHA-FACTOR applies to the alpha component of the source pixels.

DST-ALPHA-FACTOR applies to the alpha component of the destination
pixels.

ALPHA-OPERATION specifies how to combine the alpha component of the
source and destination pixels.

Possible values for factors are zero, one, src-color,
one-minus-src-color, src-alpha, one-minus-src-alpha, dst-color,
one-minus-dst-color, dst-alpha, and one-minus-dst alpha.

Possible values for operations are add, subtract, rev-subtract,
minimum, and maximum."
  (define symbol->blend-operation
    (match-lambda
      ('add          ffi:SDL_BLENDOPERATION_ADD)
      ('subtract     ffi:SDL_BLENDOPERATION_SUBTRACT)
      ('rev-subtract ffi:SDL_BLENDOPERATION_REV_SUBTRACT)
      ('minimum      ffi:SDL_BLENDOPERATION_MINIMUM)
      ('maximum      ffi:SDL_BLENDOPERATION_MAXIMUM)))
  (define symbol->blend-factor
    (match-lambda
      ('zero                ffi:SDL_BLENDFACTOR_ZERO)
      ('one                 ffi:SDL_BLENDFACTOR_ONE)
      ('src-color           ffi:SDL_BLENDFACTOR_SRC_COLOR)
      ('one-minus-src-color ffi:SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR)
      ('src-alpha           ffi:SDL_BLENDFACTOR_SRC_ALPHA)
      ('one-minus-src-alpha ffi:SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA)
      ('dst-color           ffi:SDL_BLENDFACTOR_DST_COLOR)
      ('one-minus-dst-color ffi:SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR)
      ('dst-alpha           ffi:SDL_BLENDFACTOR_DST_ALPHA)
      ('one-minus-dst-alpha ffi:SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA)))
  (let ((bitmask (ffi:sdl-compose-custom-blend-mode
                  (symbol->blend-factor src-color-factor)
                  (symbol->blend-factor dst-color-factor)
                  (symbol->blend-operation color-operation)
                  (symbol->blend-factor src-alpha-factor)
                  (symbol->blend-factor dst-alpha-factor)
                  (symbol->blend-operation alpha-operation))))
    (if (eq? bitmask ffi:SDL_BLENDMODE_INVALID)
        (sdl-error "make-blend-mode"
                   "invalid custom blend mode")
        (%make-blend-mode src-color-factor
                          dst-color-factor
                          color-operation
                          src-alpha-factor
                          dst-alpha-factor
                          alpha-operation
                          bitmask))))

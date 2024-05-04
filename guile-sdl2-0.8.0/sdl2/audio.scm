;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2022 David Thompson <davet@gnu.org>
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
;; Platform-independent audio API.
;;
;;; Code:

(define-module (sdl2 audio)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (system foreign)
  #:export (audio-device-name
            num-audio-devices))

(define* (num-audio-devices #:optional capture?)
  "Return the number of recording or playback audio devices available.
  If CAPTURE is not given or #f, the number of playback devices is
returned.  If CAPTURE? is #t, the number of recording devices is
returned."
  (ffi:sdl-get-num-audio-devices (if capture? 1 0)))

(define* (audio-device-name index #:optional capture?)
  "Return the name of the audio device at INDEX.  If CAPTURE? is not
given or #f, the name of the playback device for INDEX is returned.
If CAPTURE? is #t, the name of the recording device for INDEX is
returned."
  (let ((ptr (ffi:sdl-get-audio-device-name index (if capture? 0 1))))
    (if (null-pointer? ptr)
        #f
        (pointer->string ptr))))

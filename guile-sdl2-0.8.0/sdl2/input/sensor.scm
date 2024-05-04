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
;; Accelerometer/gyroscope input.
;;
;;; Code:

(define-module (sdl2 input sensor)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (system foreign)
  #:export (instance-id->sensor
            num-sensors
            open-sensor
            sensor-close
            sensor-device-instance-id
            sensor-device-name
            sensor-device-non-portable-type
            sensor-device-type
            sensor-instance-id
            sensor-name
            sensor-non-portable-type
            sensor-type
            update-sensors))

(define-wrapped-pointer-type <sensor>
  sensor?
  wrap-sensor unwrap-sensor
  (lambda (sensor port)
    (format port "#<sensor ~a>"
            (sensor-name sensor))))

(define (num-sensors)
  (ffi:sdl-num-sensors))

(define (sensor-device-name device-index)
  "Return the name of the sensor at DEVICE-INDEX, or #f if there is no
sensor with that index."
  (let ((ptr (ffi:sdl-sensor-get-device-name device-index)))
    (if (null-pointer? ptr)
        #f
        (pointer->string ptr))))

(define (type-enum->symbol type)
  (cond
   ((= type ffi:SDL_SENSOR_INVALID) #f)
   ((= type ffi:SDL_SENSOR_UNKNOWN) 'unknown)
   ((= type ffi:SDL_SENSOR_ACCEL) 'accelerometer)
   ((= type ffi:SDL_SENSOR_GYRO) 'gyroscope)
   ((= type ffi:SDL_SENSOR_ACCEL_L) 'accelerometer-left)
   ((= type ffi:SDL_SENSOR_GYRO_L) 'gyroscope-left)
   ((= type ffi:SDL_SENSOR_ACCEL_R) 'accelerometer-right)
   ((= type ffi:SDL_SENSOR_GYRO_R) 'gyroscope-right)))

(define (sensor-device-type device-index)
  "Return the type of the sensor at DEVICE-INDEX, or #f if there is no
sensor with that index."
  (type-enum->symbol (ffi:sdl-sensor-get-device-type device-index)))

(define (sensor-device-non-portable-type device-index)
  "Return the non-portable device type of the sensor at DEVICE-INDEX, or
#f if there is no sensor with that index."
  (let ((type (ffi:sdl-sensor-get-device-non-portable-type device-index)))
    (if (= type -1) #f type)))

(define (sensor-device-instance-id device-index)
  "Return the instance ID of the sensor at DEVICE-INDEX, or #f if there
is no sensor with that index."
  (let ((id (ffi:sdl-sensor-get-device-instance-id device-index)))
    (if (= id -1) #f id)))

(define (open-sensor device-index)
  "Return a sensor object for the sensor at DEVICE-INDEX."
  (let ((ptr (ffi:sdl-sensor-open device-index)))
    (if (null-pointer? ptr)
        (sdl-error "open-sensor" "failed to open sensor")
        (wrap-sensor ptr))))

(define (instance-id->sensor instance-id)
  "Return the sensor object associated with INSTANCE-ID."
  (let ((ptr (ffi:sdl-sensor-from-instance-id instance-id)))
    (if (null-pointer? ptr)
        (sdl-error "instance-id->sensor" "failed to get sensor")
        (wrap-sensor ptr))))

(define (sensor-name sensor)
  "Return the name of SENSOR."
  (pointer->string (ffi:sdl-sensor-get-name (unwrap-sensor sensor))))

(define (sensor-type sensor)
  "Return the type of SENSOR."
  (type-enum->symbol (ffi:sdl-sensor-get-type (unwrap-sensor sensor))))

(define (sensor-non-portable-type sensor)
  "Return the type of SENSOR."
  (let ((type (ffi:sdl-sensor-get-non-portable-type (unwrap-sensor sensor))))
    (if (= type -1) #f type)))

(define (sensor-instance-id sensor)
  "Return the instance ID of SENSOR."
  (ffi:sdl-sensor-get-instance-id (unwrap-sensor sensor)))

(define (sensor-close sensor)
  "Close SENSOR."
  (ffi:sdl-sensor-close (unwrap-sensor sensor)))

(define (update-sensors)
  "Update the current state of the open sensors."
  (ffi:sdl-sensor-update))

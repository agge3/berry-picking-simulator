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
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell
;;
;;; Code:

(use-modules (guix git)
             (guix packages)
             (guix licenses)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages sdl)
             (gnu packages texinfo))

(package
  (name "guile-sdl2")
  (version "0.8.0")
  (source (git-checkout (url (dirname (current-filename)))))
  (build-system gnu-build-system)
  (arguments
   '(#:make-flags '("GUILE_AUTO_COMPILE=0")
     #:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'bootstrap
         (lambda _ (invoke "sh" "bootstrap"))))))
  (native-inputs (list autoconf automake pkg-config texinfo))
  (inputs (list guile-3.0-latest sdl2 sdl2-image sdl2-mixer sdl2-ttf))
  (synopsis "Guile bindings for SDL2")
  (description "Guile-SDL2 provides pure Guile Scheme bindings to the SDL2 C shared
library via the foreign function interface.")
  (home-page "https://git.dthompson.us/guile-sdl2.git")
  (license lgpl3+))

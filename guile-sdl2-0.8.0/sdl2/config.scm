(define-module (sdl2 config)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:export (dynamic-link*
            %libsdl2
            %libsdl2-image
            %libsdl2-ttf
            %libsdl2-mixer))

;; Try to link against multiple library possibilities, such as the
;; absolute file name discovered by ./configure or by searching the
;; library load path as a fallback method.  Useful when restributing
;; relocatable builds.
(define (dynamic-link* names)
  (let loop ((names* names))
    (match names*
      (()
       (error "could not find library" names))
      ((name . rest)
       (or (false-if-exception (dynamic-link name))
           (loop rest))))))

;; Special case Windows since the DLL names are different.  Performing
;; this check at runtime allows a Linux machine to cross-compile
;; guile-sdl2 for a Windows target.
;;
;; Another special case is when LD_LIBRARY_PATH is set.  We *don't*
;; want to use the absolute file name of the library that pkgconfig
;; found.  Instead, we want to use just the library name so the search
;; path is used.  HOWEVER, it could be that LD_LIBRARY_PATH is set but
;; libSDL2 is not within that path BUT it is available at the absolute
;; file name provided by pkgconfig.  So, we want to prioritize
;; searching LD_LIBRARY_PATH when present, but still fallback to the
;; absolute file names.
(define %windows? (string-prefix? "Windows" (utsname:sysname (uname))))
(define-syntax-rule (define-library-name name
                      (absolute ...)
                      (relative ...)
                      (windows ...))
  (define name
    (cond
     (%windows? '(windows ...))
     ((getenv "LD_LIBRARY_PATH")
      '(relative ... absolute ...))
     (else
      '(absolute ...)))))

(define-library-name %libsdl2
  ("/usr/lib64/libSDL2")
  ("libSDL2" "libSDL2-2.0")
  ("SDL2"))
(define-library-name %libsdl2-image
  ("/libSDL2_image")
  ("libSDL2_image" "libSDL2_image-2.0")
  ("SDL2_image"))
(define-library-name %libsdl2-ttf
  ("/libSDL2_ttf")
  ("libSDL2_ttf" "libSDL2_ttf-2.0")
  ("SDL2_ttf"))
(define-library-name %libsdl2-mixer
  ("/libSDL2_mixer")
  ("libSDL2_mixer" "libSDL2_mixer-2.0")
  ("SDL2_mixer"))

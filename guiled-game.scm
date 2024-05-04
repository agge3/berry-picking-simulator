(use-modules ((sdl2)          #:prefix SDL:)
             ((sdl2 render)   #:prefix SDL:)
             ((sdl2 surface)  #:prefix SDL:)
             ((sdl2 video)    #:prefix SDL:))
(use-modules ((sdl2 bindings) #:prefix ffi:)) 

(define (game-loop)

(define (handle-events)
  (let ((event (SDL:poll-event)))
    (if event
      (cond
        ((SDL:quit-event? event)
         (display "Game exited\n"))
        ; keyboard down is key pressed, keyboard up is key released
        ((SDL:keyboard-down-event? event)
         ; handle key pressed
         (let ((key (SDL:keyboard-event-scancode event))
              (mod (SDL:keyboard-event-modifiers event)))
           (cond
             ((eq? key 'up)
              (display "Move UP\n"))
             ((eq? key 'down)
              (display "Move DOWN\n"))
             ((eq? key 'left)
              (display "Move LEFT\n"))
             ((eq? key 'right)
              (display "Move RIGHT\n")))
           ; decrease window size - unimplemented
           ;((or (eq? key 'keypad-minus)
           ;     (eq? key 'minus))
           ; (when (and (> (SDL:rect-w d-rect) 5) (> (SDL:rect-h d-rect) 5))
           ;   (display "Window size decreased\n")
           ;   (SDL:rect-w-set! d-rect (- (SDL:rect-w d-rect) 5))
           ;   (SDL:rect-h-set! d-rect (- (SDL:rect-h d-rect) 5))))

        )
        ((SDL:keyboard-up-event? event)
         ; do stuff
        )

(define (hello-sdl)
  (catch 'sdl-error
         (lambda ()
           (SDL:sdl-init '(video))
           (let ((win (SDL:make-window #:title "Guiled Game"
                                          #:position 
                                          (list ffi:SDL_WINDOWPOS_CENTERED
                                                ffi:SDL_WINDOWPOS_CENTERED)
                                          #:size '(640 480)
                                          #:show? #t)))
             ; (make-renderer win #:flags='(see doc 3.7 rendering))
             (let ((ren (SDL:make-renderer win)))
               (SDL:set-renderer-draw-color! ren 255 0 0 255)
               (SDL:clear-renderer ren)
               (SDL:present-renderer ren)
               (sleep 5)
               )))
         (lambda (key . args)
           (format #t "Have exception '~a' with detail info: ~{ ~A ~}~%" key args))
         )
  (SDL:sdl-quit)
  (display "End of SDL!\n")
  (sleep 5)
)

(hello-sdl)

;(define (draw render)
;  (let* ((surface (make-rgb-surface(640 480 8)) ; res: (640,480), bit-depth: 8
;                  (color r 255)
;                  (color g 0)
;                  (color b 0)
;                  (color a 255))
;         (texture (surface->texture render surface)))
;    (clear-renderer render)
;    (render-copy render texture)
;    (present-renderer render)
;    (sleep 2)))


;(call-with-window (make-window)
;                  (lambda (window)
;                    (call-with-renderer (make-renderer window) draw))
;
;(sdl-quit)

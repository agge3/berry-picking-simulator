(use-modules ((sdl2)          #:prefix SDL:)
             ((sdl2 render)   #:prefix SDL:)
             ((sdl2 surface)  #:prefix SDL:)
             ((sdl2 video)    #:prefix SDL:)
             ((sdl2 events)   #:prefix SDL:)
             ((sdl2 image)    #:prefix SDL:)
             ((sdl2 rect)     #:prefix SDL:))
(use-modules ((sdl2 bindings) #:prefix ffi:)) 
(use-modules  (rnrs bytevectors)
              (srfi srfi-9)
              (system foreign))

(define (handle-event event)
  ;(display "Handling events...\n")
  (cond
    ;((SDL:quit-event? event)
     ;(display "Game exited.\n"))

    ; keyboard-down is key pressed        
    ((SDL:keyboard-down-event? event)
     (display "Key: ")  (display (SDL:keyboard-event-key event)) 
                        (newline)
     (display "Scan: ") (display (SDL:keyboard-event-scancode event))
                        (newline)
     (display "Mod: ")  (display (SDL:keyboard-event-modifiers event))
                        (newline)

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
          (display "Move RIGHT\n"))        
       )))

    ; keyboard-up is key released
    ((SDL:keyboard-up-event? event)
     ; do stuff
    )
))

; default (no texture) render procedure
(define (render ren)
  (SDL:set-renderer-draw-color! ren 0 0 0 255)
  (SDL:clear-renderer ren)
  (SDL:present-renderer ren)
)

; AUTHOR: define-wrapped-pointer-type is from github.com
(define-wrapped-pointer-type <rect>
  rect?
  wrap-rect unwrap-rect
  (lambda (r port)
    (format port "#<rect ~x>: x:~d, y:~d, w:~d, h:~d"
            (pointer-address (unwrap-rect r))
            (rect-x r) (rect-y r)
            (rect-w r) (rect-h r))))

; draw (with texture(s)) render procedure
(define (draw ren texture)
  (SDL:clear-renderer ren)
  (let ((s-rect (SDL:make-rect 0 0 0 0))
       (d-rect (SDL:make-rect 0 0 0 0)))
    (SDL:set-rect-x! d-rect 20)
    (SDL:set-rect-y! d-rect 20)
    (SDL:set-rect-width! d-rect 20)
    (SDL:set-rect-height! d-rect 20)
  (SDL:render-copy ren texture #:srcrect (unwrap-rect s-rect))
  (SDL:present-renderer ren))
)

;(define (update)
;  ; to be implemented
;)

(define (main)
  ; init initial settings
  (let ((width 640) (height 480)
        ; more here...
        )
    (SDL:sdl-init '(video events))
    (display "SDL initialized.\n")
    ; print SDL version
    (display "SDL version: ") (display (SDL:sdl-version)) (newline)

    (SDL:call-with-window (SDL:make-window #:title "Guiled Game"
                                           #:position
                                             (list ffi:SDL_WINDOWPOS_CENTERED
                                                   ffi:SDL_WINDOWPOS_CENTERED)
                                           #:size (list width height) 
                                           #:show? #t)
      (lambda (window)
        ; with hardware acceleration, with vsync
        (SDL:call-with-renderer (SDL:make-renderer window '(accelerated vsync))
          (lambda (ren) (
            (let* ((player-surface  (SDL:load-image "player.png"))
                   (player-texture  (SDL:surface->texture ren player-surface))
                   (player-s-rect   (SDL:make-rect 0 0 0 0))
                   (player-d-rect   (SDL:make-rect 1 1 1 1)))
            (display "Run game.\n")
            (render ren)

            (let loop ((event (SDL:poll-event)))
              (if event
                (cond
                  ((SDL:quit-event? event)
                   (display "Exit game.\n")
                   (close-window! window)
                   )
                  (else
                    (display "Handling events.\n")
                    (handle-event event)
                    (draw ren player-texture)
                    (display "Done handling events.\n")
                    (loop (SDL:poll-event))
                  )))
              ;(display "No events to handle.\n")
              (draw ren player-texture)
              (loop (SDL:poll-event)))

            ))))))
    (SDL:sdl-quit)
))

(main)

;(define (init width height)
;  (display "Initializing...\n")
;  (SDL:sdl-init '(video events))
;  ; print SDL version
;  (display "SDL version: ") (display (SDL:sdl-version)) (newline)
;  (let ((win (SDL:make-window #:title "Guiled Game"
;                              #:position (list ffi:SDL_WINDOWPOS_CENTERED
;                                               ffi:SDL_WINDOWPOS_CENTERED)
;                              #:size (list width height) 
;                              #:show? #t)))
;    (let ((ren (SDL:make-renderer win '(accelerated vsync))))
;      (SDL:set-renderer-draw-color! ren 255 255 255 255)
;      ren
;      )))
              
            ;(let ((event (SDL:poll-event)))
            ;    (if event 
            ;      (if ((SDL:quit-event? event))
            ;        (display "Exit game.\n")
            ;        (close-window! window))
            ;      (else 
            ;        (handle-events event)))
            ;    (render ren)
            ;)))))))))
;          (lambda (ren)
;            (let ((event (SDL:poll-event)))
;              (if event
;                (if (SDL:quit-event? event) (close-window! window))
;                (handle-events event))
;              (render ren)
;              ))))
;    (SDL:sdl-quit)
;))

;(define (init width height)
;  (display "Initializing...\n")
;  (SDL:sdl-init '(video events))
;  (let ((win (SDL:make-window #:title "Guiled Game"
;                              #:position (list ffi:SDL_WINDOWPOS_CENTERED
;                                               ffi:SDL_WINDOWPOS_CENTERED)
;                              #:size (list width height) 
;                              #:show? #t)))
;    (let ((ren (SDL:make-renderer win '(accelerated vsync))))
;      (SDL:set-renderer-draw-color! ren 255 255 255 255)
;      ren
;      )))

;(define (main)
;  (let (
;        ;(src-rect (SDL:make-rect 0 0 0 0))
;        ;(dst-rect (SFL:make-rect 0 0 0 0))
;        (width 640) (height 480))
;    (catch #t
;           (lambda ()
;             (let* ((ren (init width height)))
;               (let loop ((event SDL:poll-event)))
;                 (if event
;                   (cond
;                     ((SDL:quit-event? event)
;
;                   (handle-events event)
;                   (render ren)
;                      ))
;             (display "Exiting game loop...\n")
;             )
;           (lambda (key . args)
;             (format #t "Have exception '~a' with detail info: ~{ ~A ~}~%" key args))
;           )
;    (display "Exiting error catching...\n")
;    )
;  (SDL:sdl-quit)
;)
;
;(main)
  
;(define (hello-sdl)
;  (catch 'sdl-error
;         (lambda ()
;           (SDL:sdl-init '(video))
;           (let ((win (SDL:make-window #:title "Guiled Game"
;                                          #:position 
;                                          (list ffi:SDL_WINDOWPOS_CENTERED
;                                                ffi:SDL_WINDOWPOS_CENTERED)
;                                          #:size '(640 480)
;                                          #:show? #t)))
;             ; (make-renderer win #:flags='(see doc 3.7 rendering))
;             (let ((ren (SDL:make-renderer win)))
;               (SDL:set-renderer-draw-color! ren 255 0 0 255)
;               (SDL:clear-renderer ren)
;               (SDL:present-renderer ren)
;               (sleep 5)
;               )))
;  (SDL:sdl-quit)
;  (display "End of SDL!\n")
;  (sleep 5)
;)
;
;(hello-sdl)

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

; decrease window size - unimplemented
           ;((or (eq? key 'keypad-minus)
           ;     (eq? key 'minus))
           ; (when (and (> (SDL:rect-w d-rect) 5) (> (SDL:rect-h d-rect) 5))
           ;   (display "Window size decreased.\n")
           ;   (SDL:rect-w-set! d-rect (- (SDL:rect-w d-rect) 5))
           ;   (SDL:rect-h-set! d-rect (- (SDL:rect-h d-rect) 5))))
           ;((or (eq? key 'keypad-plus)
           ;     (and (eq? key 'equals) (member 'left-shift map)))
           ; (begin
           ;   (display "Window size increased.\n")
           ;   (SDL:rect-w-set! d-rect (+ (SDL:rect-w d-rect) 5))
           ;   (SDL:rect-h-set! d-rect (+ (SDL:rect-h d-rect 5)))))

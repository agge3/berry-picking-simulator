; bps (Berry Picking Simulator) is a Scheme game. Pick berries.
; Copyright (C) 2024  agge3

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

;; global defines
(define WIDTH 640)
(define HEIGHT 480)
; seed random state from OS
(set! *random-state* (random-state-from-platform))

(define (handle-event event player-rect)
  ;(display "Handling events...\n")
  (cond
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
          (display "Move UP\n")
          (SDL:set-rect-y! player-rect (- (SDL:rect-y player-rect) 7)))
         ((eq? key 'down)
          (display "Move DOWN\n")
          (SDL:set-rect-y! player-rect (+ (SDL:rect-y player-rect) 7)))
         ((eq? key 'left)
          (display "Move LEFT\n")
          (SDL:set-rect-x! player-rect (- (SDL:rect-x player-rect) 7)))
         ((eq? key 'right)
          (display "Move RIGHT\n")        
          (SDL:set-rect-x! player-rect (+ (SDL:rect-x player-rect) 7)))
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

; draw (with texture(s)) render procedure
(define (draw ren texture rect list)
  (SDL:clear-renderer ren)
  (SDL:render-copy ren texture #:dstrect rect)
  (map (lambda (x)
         (if (pair? x)
           (let ((t (car x)) (r (cdr x)))
             ;(display t)
             ;(display r)
             (SDL:render-copy ren t #:dstrect r)
             )
           )
         )
       list)
  (SDL:present-renderer ren)
)

; WARNING: having issues with scope, needs better scoping of the berries list
(define (kill-berries berries player-rect)
  (map (lambda (x)
         (if (pair? x)
           (let* ((berry-rect (cdr x))
                  ; left is rect-x
                  (berry-left (SDL:rect-x berry-rect))
                  ; right is rect-x + rect-width
                  (berry-right (+ berry-left (SDL:rect-width berry-rect)))
                  ; top is rect-y
                  (berry-top (SDL:rect-y berry-rect))
                  ; bottom is rect-y + rect-height
                  (berry-bottom (+ berry-top (SDL:rect-height berry-rect)))
                  ; same for player...
                  (player-left (SDL:rect-x player-rect))
                  (player-right (+ player-left (SDL:rect-width player-rect)))
                  (player-top (SDL:rect-y player-rect))
                  (player-bottom (+ player-top (SDL:rect-height player-rect)))
                  )
             (if
               ; left-side collision, bounded from top-down or bottom-up
               (or (and (< player-left berry-right) (< player-top berry-bottom))
                   (and (< player-left berry-right) (> player-bottom berry-top)))
                 (display "Ate berry from left!\n")
                 (delete! x berries)
                 )
             (if
               ; right-side collision, bounded from top-down or bottom-up
               (or (and (> player-right berry-left) (< player-top berry-bottom))
                   (and (> player-right berry-left) (> player-bottom berry-top)))
                 (display "Ate berry from right!\n")
                 (delete! x berries)
                 )
               )))
       berries)
  (display berries)
  berries)

(define (main)
  (SDL:sdl-init '(video events))
  (display "SDL initialized.\n")
  ; print SDL version
  (display "SDL version: ") (display (SDL:sdl-version)) (newline)

  (SDL:call-with-window (SDL:make-window #:title "Guiled Game"
                                         #:position
                                           (list ffi:SDL_WINDOWPOS_CENTERED
                                                 ffi:SDL_WINDOWPOS_CENTERED)
                                         #:size (list WIDTH HEIGHT) 
                                         #:show? #t)
    (lambda (window)
      ; with hardware acceleration, with vsync
      (SDL:call-with-renderer (SDL:make-renderer window '(accelerated vsync))
        (lambda (ren) (
          ; scoped initialization context for game to run in
          (let* ((player-surface  (SDL:load-image "player.png"))
                 (player-texture  (SDL:surface->texture ren player-surface))
                 ; (make-rect x y w h): (x, y) is player spawn position
                 (player-rect     (SDL:make-rect 320 10 52 52))
                 (berries         (make-list 0))
                 )
          (display "Run game.\n")
          (render ren)

          (let loop ((event (SDL:poll-event)))
            (if event
              (cond
                ((SDL:quit-event? event)
                 (display "Exit game.\n")
                 ; NOTE: break out of recursion, no SDL cleanup
                 (exit)
                 )
                (else
                  ;(display "Handling events.\n")
                  (handle-event event player-rect)
                  
                  ;(display "Should I spawn a berry?\n")
                  ; REMARK: berries have a 1/20 chance to spawn
                  (cond
                    ((eq? 1 (random 21))
                    ; spawn berry and add to berries list
                    (let* ((berry-width (random WIDTH)) (berry-height (random HEIGHT))
                           ; all berries are 16x16px
                           (rect (SDL:make-rect berry-width berry-height 16 16)) 
                           (random-berry (random 7))
                           )
                      (cond
                        ((eq? random-berry 0)
                         (let* ((surface (SDL:load-image "red-apple.png"))
                                (texture (SDL:surface->texture ren surface)))
                           (display "I'm making a red apple...\n")
                           (set! berries (cons (cons texture rect) berries))
                           ))
                        ((eq? random-berry 1)
                         (let* ((surface (SDL:load-image "blueberries.png"))
                                (texture (SDL:surface->texture ren surface)))
                           (display "I'm making a blueberries...\n")
                           (set! berries (cons (cons texture rect) berries))
                           ))
                        ((eq? random-berry 2)
                         (let* ((surface (SDL:load-image "peach.png"))
                                (texture (SDL:surface->texture ren surface)))
                           (display "I'm making a peach...\n")
                           (set! berries (cons (cons texture rect) berries))
                           ))
                        ((eq? random-berry 3)
                         (let* ((surface (SDL:load-image "cherries.png"))
                                (texture (SDL:surface->texture ren surface)))
                           (display "I'm making cherries...\n")
                           (set! berries (cons (cons texture rect) berries))
                           ))
                        ((eq? random-berry 4)
                         (let* ((surface (SDL:load-image "pear.png"))
                                (texture (SDL:surface->texture ren surface)))
                           (display "I'm making a pear...\n")
                           (set! berries (cons (cons texture rect) berries))
                           ))
                        ((eq? random-berry 5)
                         (let* ((surface (SDL:load-image "strawberry.png"))
                                (texture (SDL:surface->texture ren surface)))
                           (display "I'm making a strawberry...\n")
                           (set! berries (cons (cons texture rect) berries))
                           ))
                        ((eq? random-berry 6)
                         (let* ((surface (SDL:load-image "avacado.png"))
                                (texture (SDL:surface->texture ren surface)))
                           (display "I'm making a avacado...\n")
                           (set! berries (cons (cons texture rect) berries))
                           ))
                        ))))

                  ; TODO: kill-berries needs to be properly implemented
                  ;(set! berries (kill-berries berries player-rect))
                  (draw ren player-texture player-rect berries)
                  (loop (SDL:poll-event))
                  )))

            ; TODO: kill-berries needs to be properly implemented
            ;(set! berries (kill-berries berries player-rect))
            (draw ren player-texture player-rect berries)
            ;(display berries)
            (loop (SDL:poll-event))))
))))))

;; entry point
(main)

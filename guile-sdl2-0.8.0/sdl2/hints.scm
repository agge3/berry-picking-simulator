;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2021 Leo Prikler <leo.prikler@student.tugraz.at>
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

(define-module (sdl2 hints)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9)
  #:use-module (sdl2)
  #:use-module ((sdl2 bindings) #:prefix ffi:)
  #:use-module (system foreign)
  #:export (get-hint set-hint!))

(define (symbol->priority sym)
  (match sym
    ('default  ffi:SDL_HINT_DEFAULT)
    ('normal   ffi:SDL_HINT_NORMAL)
    ('override ffi:SDL_HINT_OVERRIDE)))

(define (symbol->hint sym)
  (match sym
    ('accelerometer-as-joystick "SDL_ACCELEROMETER_AS_JOYSTICK")
    ('allow-topmost "SDL_ALLOW_TOPMOST")
    ('android-apk-expansion-main-file-version
     "SDL_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION")
    ('android-apk-expansion-patch-file-version
     "SDL_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION")
    ('android-separate-mouse-and-touch
     "SDL_ANDROID_SEPARATE_MOUSE_AND_TOUCH")
    ('apple-tv-ui-events "SDL_APPLE_TV_UI_EVENTS")
    ('apple-tv-remote-allow-rotation
     "SDL_APPLE_TV_REMOTE_ALLOW_ROTATION")
    ('bmp-save-legacy-format "SDL_BMP_SAVE_LEGACY_FORMAT")
    ('display-usable-bounds "SDL_DISPLAY_USABLE_BOUNDS")
    ('emscripten-keyboard-element "SDL_EMSCRIPTEN_KEYBOARD_ELEMENT")
    ('event-logging "SDL_EVENT_LOGGING")
    ('framebuffer-acceleration "SDL_FRAMEBUFFER_ACCELERATION")
    ('gamecontroller-ignore-devices "SDL_GAMECONTROLLER_IGNORE_DEVICES")
    ('gamecontroller-ignore-devices-except
     "SDL_GAMECONTROLLER_IGNORE_DEVICES_EXCEPT")
    ('gamecontroller-use-button-labels "SDL_GAMECONTROLLER_USE_BUTTON_LABELS")
    ('gamecontrollerconfig "SDL_GAMECONTROLLERCONFIG")
    ('gamecontrollerconfig-file "SDL_GAMECONTROLLERCONFIG_FILE")
    ('gamecontrollertype "SDL_GAMECONTROLLERTYPE")
    ('grab-keyboard "SDL_GRAB_KEYBOARD")
    ('idle-timer-disabled "SDL_IDLE_TIMER_DISABLED")
    ('joystick-allow-background-events "SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS")
    ('joystick-hidapi "SDL_JOYSTICK_HIDAPI")
    ('joystick-hidapi-gamecube "SDL_JOYSTICK_HIDAPI_GAMECUBE")
    ('joystick-hidapi-ps4 "SDL_JOYSTICK_HIDAPI_PS4")
    ('joystick-hidapi-ps4-rumble "SDL_JOYSTICK_HIDAPI_PS4_RUMBLE")
    ('joystick-hidapi-steam "SDL_JOYSTICK_HIDAPI_STEAM")
    ('joystick-hidapi-steam-controllers
     "SDL_JOYSTICK_HIDAPI_STEAM_CONTROLLERS")
    ('joystick-hidapi-switch "SDL_JOYSTICK_HIDAPI_SWITCH")
    ('joystick-hidapi-xbox "SDL_JOYSTICK_HIDAPI_XBOX")
    ('mac-background-app "SDL_MAC_BACKGROUND_APP")
    ('mac-ctrl-click-emulate-right-click
     "SDL_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK")
    ('mouse-double-click-radius "SDL_MOUSE_DOUBLE_CLICK_RADIUS")
    ('mouse-focus-clickthrough "SDL_MOUSE_FOCUS_CLICKTHROUGH")
    ('mouse-normal-speed-scale "SDL_MOUSE_NORMAL_SPEED_SCALE")
    ('mouse-relative-mode-warp "SDL_MOUSE_RELATIVE_MODE_WARP")
    ('mouse-relative-speed-scale "SDL_MOUSE_RELATIVE_SPEED_SCALE")
    ('mouse-touch-events "SDL_MOUSE_TOUCH_EVENTS")
    ('no-signal-handlers "SDL_NO_SIGNAL_HANDLERS")
    ('orientations "SDL_ORIENTATIONS")
    ('render-batching "SDL_RENDER_BATCHING")
    ('render-direct3d11-debug "SDL_RENDER_DIRECT3D11_DEBUG")
    ('render-direct3d-threadsafe "SDL_RENDER_DIRECT3D_THREADSAFE")
    ('render-driver "SDL_RENDER_DRIVER")
    ('render-logical-size-mode "SDL_RENDER_LOGICAL_SIZE_MODE")
    ('render-opengl-shaders "SDL_RENDER_OPENGL_SHADERS")
    ('render-scale-quality "SDL_RENDER_SCALE_QUALITY")
    ('render-vsync "SDL_RENDER_VSYNC")
    ('return-key-hides-ime "SDL_RETURN_KEY_HIDES_IME")
    ('rpi-video-layer "SDL_RPI_VIDEO_LAYER")
    ('qtwayland-content-orientation "SDL_QTWAYLAND_CONTENT_ORIENTATION")
    ('qtwayland-window-flags "SDL_QTWAYLAND_WINDOW_FLAGS")
    ('thread-stack-size "SDL_THREAD_STACK_SIZE")
    ('timer-resolution "SDL_TIMER_RESOLUTION")
    ('touch-mouse-events "SDL_TOUCH_MOUSE_EVENTS")
    ('video-allow-screensaver "SDL_VIDEO_ALLOW_SCREENSAVER")
    ('video-external-context "SDL_VIDEO_EXTERNAL_CONTEXT")
    ('video-highdpi-disabled "SDL_VIDEO_HIGHDPI_DISABLED")
    ('video-mac-fullscreen-spaces "SDL_VIDEO_MAC_FULLSCREEN_SPACES")
    ('video-minimize-on-focus-loss "SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS")
    ('video-window-share-pixel-format "SDL_VIDEO_WINDOW_SHARE_PIXEL_FORMAT")
    ('video-win-d3dcompiler "SDL_VIDEO_WIN_D3DCOMPILER")
    ('video-x11-force-egl "SDL_VIDEO_X11_FORCE_EGL")
    ('video-x11-net-wm-ping "SDL_VIDEO_X11_NET_WM_PING")
    ('video-x11-window-visualid "SDL_VIDEO_X11_WINDOW_VISUALID")
    ('video-x11-xinerama "SDL_VIDEO_X11_XINERAMA")
    ('video-x11-xrandr "SDL_VIDEO_X11_XRANDR")
    ('video-x11-xvidmode "SDL_VIDEO_X11_XVIDMODE")
    ('wave-fact-chunk "SDL_WAVE_FACT_CHUNK")
    ('wave-riff-chunk-size "SDL_WAVE_RIFF_CHUNK_SIZE")
    ('wave-truncation "SDL_WAVE_TRUNCATION")
    ('windows-disable-thread-naming "SDL_WINDOWS_DISABLE_THREAD_NAMING")
    ('windows-enable-message-loop "SDL_WINDOWS_ENABLE_MESSAGE_LOOP")
    ('windows-intresource-icon "SDL_WINDOWS_INTRESOURCE_ICON")
    ('windows-intresource-icon-small "SDL_WINDOWS_INTRESOURCE_ICON_SMALL")
    ('windows-no-close-on-alt-f4 "SDL_WINDOWS_NO_CLOSE_ON_ALT_F4")
    ('window-frame-usable-while-cursor-hidden
     "SDL_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN")
    ('winrt-handle-back-button "SDL_WINRT_HANDLE_BACK_BUTTON")
    ('winrt-privacy-policy-label "SDL_WINRT_PRIVACY_POLICY_LABEL")
    ('winrt-privacy-policy-url "SDL_WINRT_PRIVACY_POLICY_URL")
    ('xinput-enabled "SDL_XINPUT_ENABLED")
    ('xinput-use-old-joystick-mapping "SDL_XINPUT_USE_OLD_JOYSTICK_MAPPING")))

(define* (get-hint hint)
  (let* ((hint* (string->pointer (symbol->hint hint)))
         (value* (ffi:sdl-get-hint hint*)))
    (and (not (null-pointer? value*))
         (pointer->string value*))))

(define* (set-hint! hint value #:optional (priority 'normal))
  (let ((hint* (string->pointer (symbol->hint hint)))
        (value* (string->pointer
                 (match value
                   (#t "1")
                   (#f "0")
                   ((? number? n) (number->string value))
                   ((? string? s) s))))
        (priority (symbol->priority priority)))
    (= (ffi:sdl-set-hint-with-priority hint* value* priority) 1)))

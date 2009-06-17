;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; keyboard-scancodes.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the clois-lane root directory for more info.
;;;;
;;;; Keyboard scancodes pulled from OISKeyboard.h

(in-package :clois-lane)


;; defparameter because ASDF spazzes out on a defconstant
(defvar +scancodes+
  '((#x00 . :kc-unassigned)
    (#x01 . :kc-escape)
    (#x02 . :kc-1)
    (#x03 . :kc-2)
    (#x04 . :kc-3)
    (#x05 . :kc-4)
    (#x06 . :kc-5)
    (#x07 . :kc-6)
    (#x08 . :kc-7)
    (#x09 . :kc-8)
    (#x0a . :kc-9)
    (#x0b . :kc-0)
    (#x0c . :kc-minus)          ; - on main keyboard
    (#x0d . :kc-equals)
    (#x0e . :kc-back)           ; Backspace
    (#x0f . :kc-tab)
    (#x10 . :kc-q)
    (#x11 . :kc-w)
    (#x12 . :kc-e)
    (#x13 . :kc-r)
    (#x14 . :kc-t)
    (#x15 . :kc-y)
    (#x16 . :kc-u)
    (#x17 . :kc-i)
    (#x18 . :kc-o)
    (#x19 . :kc-p)
    (#x1a . :kc-lbracket)
    (#x1b . :kc-rbracket)
    (#x1c . :kc-return)         ; Enter on main keyboard
    (#x1d . :kc-lcontrol)
    (#x1e . :kc-a)
    (#x1f . :kc-s)
    (#x20 . :kc-d)
    (#x21 . :kc-f)
    (#x22 . :kc-g)
    (#x23 . :kc-h)
    (#x24 . :kc-j)
    (#x25 . :kc-k)
    (#x26 . :kc-l)
    (#x27 . :kc-semicolon)
    (#x28 . :kc-apostrophe)
    (#x29 . :kc-grave)          ; accent
    (#x2a . :kc-lshift)
    (#x2b . :kc-backslash)
    (#x2c . :kc-z)
    (#x2d . :kc-x)
    (#x2e . :kc-c)
    (#x2f . :kc-v)
    (#x30 . :kc-b)
    (#x31 . :kc-n)
    (#x32 . :kc-m)
    (#x33 . :kc-comma)
    (#x34 . :kc-period)         ; . on main keyboard
    (#x35 . :kc-slash)          ; / on main keyboard
    (#x36 . :kc-rshift)
    (#x37 . :kc-multiply)       ; * on numeric keypad
    (#x38 . :kc-lmenu)          ; left Alt
    (#x39 . :kc-space)
    (#x3a . :kc-capital)
    (#x3b . :kc-f1)
    (#x3c . :kc-f2)
    (#x3d . :kc-f3)
    (#x3e . :kc-f4)
    (#x3f . :kc-f5)
    (#x40 . :kc-f6)
    (#x41 . :kc-f7)
    (#x42 . :kc-f8)
    (#x43 . :kc-f9)
    (#x44 . :kc-f10)
    (#x45 . :kc-numlock)
    (#x46 . :kc-scroll)         ; Scroll Lock
    (#x47 . :kc-numpad7)
    (#x48 . :kc-numpad8)
    (#x49 . :kc-numpad9)
    (#x4a . :kc-subtract)       ; - on numeric keypad
    (#x4b . :kc-numpad4)
    (#x4c . :kc-numpad5)
    (#x4d . :kc-numpad6)
    (#x4e . :kc-add)            ; + on numeric keypad
    (#x4f . :kc-numpad1)
    (#x50 . :kc-numpad2)
    (#x51 . :kc-numpad3)
    (#x52 . :kc-numpad0)
    (#x53 . :kc-decimal)        ; . on numeric keypad
    (#x56 . :kc-oem_102)        ; < > | on UK/Germany keyboards
    (#x57 . :kc-f11)
    (#x58 . :kc-f12)
    (#x64 . :kc-f13)            ; (NEC PC98)
    (#x65 . :kc-f14)            ; (NEC PC98)
    (#x66 . :kc-f15)            ; (NEC PC98)
    (#x70 . :kc-kana)           ; (Japanese keyboard)
    (#x73 . :kc-abnt_c1)        ; / ? on Portugese / Brazilian keyboards
    (#x79 . :kc-convert)        ; (Japanese keyboard)
    (#x7b . :kc-noconvert)      ; (Japanese keyboard)
    (#x7d . :kc-yen)            ; (Japanese keyboard)
    (#x7e . :kc-abnt_c2)        ; Numpad . on Portugese / Brazilian kbd
    (#x8d . :kc-numpadequals+)  ; = on numeric keypad (NEC PC98)
    (#x90 . :kc-prevtrack)      ; (KC-CIRCUMFLEX on Japanese keyboard)
    (#x91 . :kc-at)             ; (NEC PC98)
    (#x92 . :kc-colon)          ; (NEC PC98)
    (#x93 . :kc-underline)      ; (NEC PC98)
    (#x94 . :kc-kanji)          ; (Japanese keyboard)
    (#x95 . :kc-stop)           ; (NEC PC98)
    (#x96 . :kc-ax)             ; (Japan AX)
    (#x97 . :kc-unlabeled)      ; (J3100)
    (#x99 . :kc-nexttrack)      ; Next Track
    (#x9c . :kc-numpadenter)    ; Enter on numeric keypad
    (#x9d . :kc-rcontrol)
    (#xa0 . :kc-mute)           ; Mute
    (#xa1 . :kc-calculator)     ; Calculator
    (#xa2 . :kc-playpause)      ; Play / Pause
    (#xa4 . :kc-mediastop)      ; Media Stop
    (#xae . :kc-volumedown)     ; Volume -
    (#xb0 . :kc-volumeup)       ; Volume +
    (#xb2 . :kc-webhome)        ; Web home
    (#xb3 . :kc-numpadcomma)    ; , on numeric keypad (NEC PC98)
    (#xb5 . :kc-divide)         ; / on numeric keypad
    (#xb7 . :kc-sysrq)
    (#xb8 . :kc-rmenu)          ; right Alt
    (#xc5 . :kc-pause)          ; Pause
    (#xc7 . :kc-home)           ; Home on arrow keypad
    (#xc8 . :kc-up)             ; UpArrow on arrow keypad
    (#xc9 . :kc-pgup)           ; PgUp on arrow keypad
    (#xcb . :kc-left)           ; LeftArrow on arrow keypad
    (#xcd . :kc-right)          ; RightArrow on arrow keypad
    (#xcf . :kc-end)            ; End on arrow keypad
    (#xd0 . :kc-down)           ; DownArrow on arrow keypad
    (#xd1 . :kc-pgdown)         ; PgDn on arrow keypad
    (#xd2 . :kc-insert)         ; Insert on arrow keypad
    (#xd3 . :kc-delete)         ; Delete on arrow keypad
    (#xdb . :kc-lwin)           ; Left Windows key
    (#xdc . :kc-rwin)           ; Right Windows key
    (#xdd . :kc-apps)           ; AppMenu key
    (#xde . :kc-power)          ; System Power
    (#xdf . :kc-sleep)          ; System Sleep
    (#xe3 . :kc-wake)           ; System Wake
    (#xe5 . :kc-websearch)      ; Web Search
    (#xe6 . :kc-webfavorites+)  ; Web Favorites
    (#xe7 . :kc-webrefresh)     ; Web Refresh
    (#xe8 . :kc-webstop)        ; Web Stop
    (#xe9 . :kc-webforward)     ; Web Forward
    (#xea . :kc-webback)        ; Web Back
    (#xeb . :kc-mycomputer)     ; My Computer
    (#xec . :kc-mail)           ; Mail
    (#xed . :kc-mediaselect)))  ; Media Select

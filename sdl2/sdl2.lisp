(in-package :skitter.sdl2)

;;--------------------------------------------
;; scancode lookup

(defvar *window-event-names*
  #(:none :shown :hidden :exposed :moved :resized
    :size-changed :minimized :maximized :restored
    :enter :leave :focus-gained :focus-lost :close
    :take-focus :hit-test))

(defvar *mouse-button-names*
  #(:0 :left :middle :right :other0 :other1 :other2 :other3 :other4))

(defvar *key-button-names*
  #(:unknown nil nil nil :a :b
    :c :d :e :f
    :g :h :i :j
    :k :l :m :n
    :o :p :q :r
    :s :t :u :v
    :w :x :y :z
    :1 :2 :3 :4
    :5 :6 :7 :8
    :9 :0 :return :escape
    :backspace :tab :space
    :minus :equals :leftbracket
    :rightbracket :backslash :nonushash
    :semicolon :apostrophe :grave
    :comma :period :slash
    :capslock :f1 :f2 :f3
    :f4 :f5 :f6 :f7
    :f8 :f9 :f10 :f11
    :f12 :printscreen :scrolllock
    :pause :insert :home
    :pageup :delete :end
    :pagedown :right :left
    :down :up :numlockclear
    :kp_divide :kp_multiply :kp_minus
    :kp_plus :kp_enter :kp_1
    :kp_2 :kp_3 :kp_4 :kp_5
    :kp_6 :kp_7 :kp_8 :kp_9
    :kp_0 :kp_period :nonusbackslash
    :application :power :kp_equals
    :f13 :f14 :f15 :f16
    :f17 :f18 :f19 :f20
    :f21 :f22 :f23 :f24
    :execute :help :menu
    :select :stop :again
    :undo :cut :copy :paste
    :find :mute :volumeup
    :volumedown :lockingcapslock
    :lockingnumlock :lockingscrolllock
    :kp_comma :kp_equalsas400
    :international1 :international2
    :international3 :international4
    :international5 :international6
    :international7 :international8
    :international9 :lang1 :lang2
    :lang3 :lang4 :lang5
    :lang6 :lang7 :lang8
    :lang9 :alterase :sysreq
    :cancel :clear :prior
    :return2 :separator :out
    :oper :clearagain :crsel
    :exsel nil nil nil nil nil nil nil nil nil nil nil
    :kp_00 :kp_000 :thousandsseparator
    :decimalseparator :currencyunit
    :currencysubunit :kp_leftparen
    :kp_rightparen :kp_leftbrace
    :kp_rightbrace :kp_tab :kp_backspace
    :kp_a :kp_b :kp_c :kp_d
    :kp_e :kp_f :kp_xor
    :kp_power :kp_percent :kp_less
    :kp_greater :kp_ampersand
    :kp_dblampersand :kp_verticalbar
    :kp_dblverticalbar :kp_colon :kp_hash
    :kp_space :kp_at :kp_exclam
    :kp_memstore :kp_memrecall
    :kp_memclear :kp_memadd
    :kp_memsubtract :kp_memmultiply
    :kp_memdivide :kp_plusminus :kp_clear
    :kp_clearentry :kp_binary :kp_octal
    :kp_decimal :kp_hexadecimal nil nil
    :lctrl :lshift :lalt
    :lgui :rctrl :rshift
    :ralt :rgui nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :mode :audionext :audioprev
    :audiostop :audioplay :audiomute
    :mediaselect :www :mail
    :calculator :computer :ac_search
    :ac_home :ac_back :ac_forward
    :ac_stop :ac_refresh :ac_bookmarks
    :brightnessdown :brightnessup
    :displayswitch :kbdillumtoggle
    :kbdillumdown :kbdillumup :eject
    :sleep))

;;--------------------------------------------
;; sdl timestamp conversion

;; {TODO} optimize
(let ((sdl->lisp-time-offset 0))
  (defun set-sdl->lisp-time-offset ()
    (setf sdl->lisp-time-offset (cl:- (get-internal-real-time) (sdl2::get-ticks))))
  (defun sdl->lisp-time (sdl-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (cl:+ sdl-time sdl->lisp-time-offset))
  (defun lisp->sdl-time (lisp-time)
    (when (= sdl->lisp-time-offset 0)
      (set-sdl->lisp-time-offset))
    (cl:- lisp-time sdl->lisp-time-offset)))

;;--------------------------------------------
;; sdl event helpers

(defmacro %case-events ((event &key (method :poll) (timeout nil))
                        &body event-handlers)
  `(let (,(when (symbolp event) `(,event (sdl2:new-event))))
     (loop :until (= 0  (sdl2:next-event ,event ,method ,timeout)) :do
        (case (sdl2::get-event-type ,event)
          ,@(loop :for (type params . forms) :in event-handlers
               :append (let ((type (if (listp type)
				       type
				       (list type))))
                         (loop :for typ :in type :collect
                            (sdl2::expand-handler event typ params forms)))
               :into results
               :finally (return (remove nil results)))))
     (sdl2:free-event ,event)))

(defun collect-sdl-events (win &optional tpref)
  (declare (ignore win))
  (%case-events (event)
    (:quit
     (:timestamp ts)
     (skitter:apply-state (system-quitting +system+) (sdl->lisp-time ts) tpref
                          :is t))

    (:windowevent
     (:timestamp ts :event e :data1 x :data2 y)
     (let ((action (aref *window-event-names* e))
	   (ts (sdl->lisp-time ts))
	   (win (window 0)))
       (case action
	 (:moved (apply-pos-2d (window-pos win) ts tpref
                               :vec (v!int x y)))
	 (:resized (apply-size-2d (window-size win) ts tpref
                                  :vec (v!int x y)))
	 (:size-changed (apply-size-2d (window-size win) ts tpref
                                       :vec (v!int x y)))
	 (:minimized (apply-layout (window-layout win) ts tpref
                                   :state :minimized))
	 (:maximized (apply-layout (window-layout win) ts tpref
                                   :state :maximized))
	 (:restored (apply-layout (window-layout win) ts tpref
                                  :state :restored))
	 (:close (apply-state (window-layout win) ts tpref
                              :is t)))))

    (:mousewheel
     (:timestamp ts :which id :x x :y y)
     (let ((mouse (mouse id)))
       (apply-xy-wheel (mouse-wheel mouse)
		       (sdl->lisp-time ts)
                       tpref
		       :vec (v! x y))))

    ((:mousebuttondown :mousebuttonup)
     (:timestamp ts :which id :button b :state s :x x :y y)
     ;; what should we do with clicks? (:clicks c)
     (let ((mouse (mouse id)))
       (apply-button (mouse-button mouse b)
		     (sdl->lisp-time ts)
                     tpref
		     :down-p (= 1 s))
       (apply-xy-pos (mouse-pos mouse)
		     (sdl->lisp-time ts)
                     tpref
		     :vec (v! x y))))

    (:mousemotion
     (:timestamp ts :which id :x x :y y :xrel xrel :yrel yrel)
     ;; what should we do with state? (:state s)
     (let ((mouse (mouse id)))
       (apply-xy-pos (mouse-pos mouse)
		     (sdl->lisp-time ts)
                     tpref
		     :vec (v! x y)
		     :relative (v! xrel yrel))))

    ((:keydown :keyup)
     (:timestamp ts :state s :keysym keysym)
     ;; what should we do with repeat (:repeat r)
     (let ((kbd (keyboard 0)))
       (apply-button (keyboard-button
		      kbd (plus-c:c-ref keysym sdl2-ffi:sdl-keysym :scancode))
		     (sdl->lisp-time ts)
                     tpref
		     :down-p (= 1 s))))))

;;--------------------------------------------
;; intializing

(defmethod initialize-kind :after ((kind keyboard))
  (loop for i across *key-button-names* do
       (identity i)
       (add kind (make-button))))

(defmethod initialize-kind :after ((kind mouse))
  (loop for i across *mouse-button-names* do
       (identity i)
       (add kind (make-button))))

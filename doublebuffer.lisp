;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;; doublebuffer X11 extension (xdbe) protocol implementation
;;; based on dbelib.html [1], dbeproto.h [2] & dbe.h [3]
;;; [1] http://www.x.org/releases/X11R7.6/doc/libXext/dbelib.html
;;; [2] http://cgit.freedesktop.org/xorg/proto/xextproto/tree/dbeproto.h
;;; [3] http://cgit.freedesktop.org/xorg/proto/xextproto/tree/dbe.h
;;; written by Mikael K. "InvalidCo" Harvilahti
;;; <mikael.harvilahti@gmail.com>

;;; LICENSE: none/pd/wtfpl

;;; TODO: this does not implement all of the possible requests
;;; (extras like XdbeGetVisualInfo and XdbeGetBackBufferAttributes)

(in-package :xlib)

(define-extension "DOUBLE-BUFFER"
    :errors (bad-backbuffer))

(export '(bad-backbuffer
	  backbuffer
	  create-backbuffer
	  destroy-backbuffer
	  swap-backbuffers
	  swap-backbuffer
	  begin-backbuffer-idiom
	  end-backbuffer-idiom
	  with-backbuffer-idiom))

(pushnew :clx-ext-backbuffer *features*)

(define-condition bad-backbuffer (request-error) ())
(define-error bad-backbuffer decode-core-error)

(def-clx-class (backbuffer (:include drawable) (:copier nil)
			   (:print-function print-drawable)))

(defun encode-swap-action (action)
  (ecase action
    (:undefined 0)
    (:background 1)
    (:untouched 2)
    (:copied 3)))

(defun backbuffer-query-version (display)
  (with-buffer-request-and-reply
      (display (extension-opcode display "DOUBLE-BUFFER") 16)
      ((card16 0))
    (values
     (card8-get 8)
     (card8-get 9))))

(defun create-backbuffer (window &optional (swap-action-hint :untouched))
  (let* ((display (window-display window))
	 (backbuffer-id (resourcealloc display))
	 (backbuffer (make-backbuffer :display display)))
    (setf (backbuffer-id backbuffer) backbuffer-id)
    (with-buffer-request
	(display (extension-opcode display "DOUBLE-BUFFER"))
      (data 1) ;; X_DbeAllocateBackBufferName
      (window window)
      (resource-id backbuffer-id)
      (card8 (encode-swap-action swap-action-hint))
      (card8 0)
      (card16 0))
    backbuffer))

(defun destroy-backbuffer (backbuffer)
  (let* ((display (backbuffer-display backbuffer)))
    (with-buffer-request
	(display (extension-opcode display "DOUBLE-BUFFER"))
      (data 2) ;; X_DbeDeallocateBackBufferName
      (resource-id (backbuffer-id backbuffer)))))

;;; NOTE: untested for the MSB-FIRST scenario, should work.
(defun swap-backbuffers (list-of-windows-and-actions &optional (display (window-display (first (first list-of-windows-and-actions)))))
  "Swap the backbuffers of WINDOWS using ACTIONS.
   LIST-OF-WINDOWS-AND-ACTIONS =
     ((window action)
      (window action)...
   Each WINDOW is a WINDOW which has an associated BACKBUFFER.
   Pass the function the WINDOW and NOT the BACKBUFFER.
   Each ACTION is one of the following
    :BACKGROUND - the backbuffer is cleared with the bg-color property it has
    :UNTOUCHED - the backbuffer is left untouched
    :COPIED - the contents of the frontbuffer are copied to the backbuffer

   DISPLAY is an optional argument specifying a DISPLAY. This function works only on one (1) display at a time. The option of emitting DISPLAY is a convenience - it defaults to the first window's display.

   Once this call is made, the visible portion (frontbuffer) and the non-visible portion (backbuffer) switch places. After that, appropriate action is taken (the effects of the action taken can only be seen after calling SWAP-BACKBUFFERS for a second time."
  (when (not display)
    (setf display (window-display (first (first list-of-windows-and-actions)))))
  (with-buffer-request
      (display (extension-opcode display "DOUBLE-BUFFER"))
    (data 3) ;; X_DbeSwapBuffers
    (card32 (length list-of-windows-and-actions))
    ;;; The protocol header explicitly specifies swap-action followed
    ;;; by 3 padding bytes, endianness or no endianness.
    ;;; CLX's protocol specifying protocol is not exactly very clear,
    ;;; (there might be some alternate way of specifying complex seqs)
    ;;; so here we pad the bytes manually if we are using the :msbfirst
    ;;; protocol. If left to it's own devices, CLX would pad those
    ;;; bytes and put a zero in the first byte (which is where X11
    ;;; expects to find the swap-action) Putting the swap-action byte
    ;;; last, where it will be dismissed as padding.
    ((sequence :format int32)
     (mapcan #'(lambda (x)
		 (list (window-id (first x))
		       (*
			(if (eq (display-byte-order display) :lsbfirst)
			    1 ;; don't pad
			    (expt 2 24)) ;;pad by 3 bytes
			(encode-swap-action (second x)))))
	     list-of-windows-and-actions))))

(defun swap-backbuffer (window action)
  "This is a convenience function with a saner calling convention. Most people (should) use this. See SWAP-BACKBUFFERS for details."
  (swap-backbuffers (list (list window action))))

(defun begin-backbuffer-idiom (display)
  (with-buffer-request (display (extension-opcode display "DOUBLE-BUFFER"))
    (data 4))) ;; X_DbeBeginIdom [sic]

(defun end-backbuffer-idiom (display)
  (with-buffer-request (display (extension-opcode display "DOUBLE-BUFFER"))
    (data 5))) ;; X_DbeEndIdom

(defmacro with-backbuffer-idiom ((display) &body body)
  (let ((dpy-sym (gensym "DISPLAY")))
    `(let ((,dpy-sym ,display))
       (unwind-protect
	    (progn
	      (begin-backbuffer-idiom ,dpy-sym)
	      ,@body)
	 (end-backbuffer-idiom ,dpy-sym)))))

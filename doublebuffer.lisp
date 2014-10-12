;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-

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
	  swap-backbuffer))

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

(defun swap-backbuffers (display list-of-windows-and-actions)
  (with-buffer-request
      (display (extension-opcode display "DOUBLE-BUFFER"))
    (data 3) ;; X_DbeSwapBuffers
    (card32 (length list-of-windows-and-actions))
    ;; TODO: check the byte order of ACTIONs...
    ;; possibility for an odd bug (dbeproto.h says swapAction is
    ;; padded with 3 empty bytes, but this function here just assumes
    ;; clx goes LSB always. (in MSB mode, CLX probably puts the padded
    ;; zeroes before the ACTION.
    ((sequence :format int32) (mapcan #'(lambda (x)
					  (list (window-id (first x)) (encode-swap-action (second x))))
				      list-of-windows-and-actions))))

(defun swap-backbuffer (window action)
  (swap-backbuffers (window-display window) (list (list window action))))

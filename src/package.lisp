;;; package.lisp --- Package definition for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

;;; Configure CFFI to find shared libraries in /usr/lib{64}/icl/
;;; This runs before osicat is loaded and registers a restore hook
;;; for when the saved image is started.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :cffi)
    (let ((dirs-sym (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" :cffi)))
      (pushnew #P"/usr/lib64/icl/" (symbol-value dirs-sym) :test #'equal)
      (pushnew #P"/usr/lib/icl/" (symbol-value dirs-sym) :test #'equal))))

(uiop:register-image-restore-hook
 (lambda ()
   (when (find-package :cffi)
     (let ((dirs-sym (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" :cffi)))
       (pushnew #P"/usr/lib64/icl/" (symbol-value dirs-sym) :test #'equal)
       (pushnew #P"/usr/lib/icl/" (symbol-value dirs-sym) :test #'equal))))
 nil)  ; nil = run early (before other hooks)

(defpackage #:icl
  (:documentation "Interactive Common Lisp - an enhanced REPL for Common Lisp.")
  (:use #:cl)
  (:export
   ;; Main entry points
   #:main
   #:run-repl
   #:start-repl

   ;; Configuration
   #:*icl-package*
   #:*prompt-string*
   #:*continuation-prompt*
   #:*result-prefix*
   #:*history-file*
   #:*history-size*
   #:*config-file*
   #:*use-multiline-editor*
   #:*colors-enabled*

   ;; Hooks
   #:*before-eval-hook*
   #:*after-eval-hook*
   #:*prompt-hook*
   #:*error-hook*

   ;; History variables (ICL-prefixed to avoid shadowing)
   #:icl-*
   #:icl-**
   #:icl-***
   #:icl-+
   #:icl-++
   #:icl-+++
   #:icl-/
   #:icl-//
   #:icl-///

   ;; IRB-style history shortcuts
   #:_
   #:__
   #:___

   ;; Command system
   #:define-command
   #:find-command
   #:list-commands

   ;; Slynk/Backend configuration
   #:*use-slynk*
   #:*slynk-port*
   #:*slynk-host*
   #:*default-lisp*
   #:*slynk-connected-p*
   #:slynk-connect
   #:slynk-disconnect
   #:start-inferior-lisp
   #:stop-inferior-lisp))

;;; icl.el --- Emacs integration for ICL browser -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anthony Green <green@moxielogic.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides integration between Sly and ICL's browser visualization interface.
;; Start ICL browser with M-x icl-browser when you have an active Sly connection.

;;; Code:

(require 'sly)

(defgroup icl nil
  "ICL browser integration."
  :group 'sly)

(defcustom icl-program "icl"
  "Path to the ICL executable."
  :type 'string
  :group 'icl)

(defvar icl--slynk-port nil
  "Port of the Slynk server created for ICL.")

(defun icl--setup-eval-hook ()
  "Set up eval generation counter in the Lisp image."
  ;; All symbols must be fully qualified to CL-USER since slynk reads
  ;; forms in its own package context
  (sly-eval '(cl:progn
              (cl:defvar cl-user::*icl-eval-generation* 0)
              ;; Define the hook function
              (cl:defun cl-user::icl-increment-eval-generation ()
                (cl:incf cl-user::*icl-eval-generation*))
              ;; Only add the hook if not already present
              ;; Use slynk:: (internal) since *pre-reply-hook* is not exported
              (cl:unless (cl:member (cl:quote cl-user::icl-increment-eval-generation)
                                    slynk::*pre-reply-hook*)
                (cl:push (cl:quote cl-user::icl-increment-eval-generation)
                         slynk::*pre-reply-hook*))
              t)))

(defun icl-browser ()
  "Start ICL browser connected to the current Sly session."
  (interactive)
  (unless (sly-current-connection)
    (user-error "No active Sly connection"))
  (when (get-process "icl-browser")
    (user-error "ICL browser already running. Use M-x icl-browser-stop first"))
  ;; Set up eval generation counter for visualization refresh
  (icl--setup-eval-hook)
  ;; Create a Slynk server that accepts connections
  (let ((port (sly-eval '(slynk:create-server :port 0 :dont-close t))))
    (setq icl--slynk-port port)
    (message "ICL connecting on port %d" port)
    (start-process "icl-browser" "*icl-browser*" icl-program
                   "--connect" (format "localhost:%d" port) "-b")))

(defun icl-browser-stop ()
  "Stop ICL browser process."
  (interactive)
  (when-let ((proc (get-process "icl-browser")))
    (kill-process proc)
    (message "ICL browser stopped")))

(defun icl-browser-restart ()
  "Restart ICL browser."
  (interactive)
  (icl-browser-stop)
  (run-at-time 0.5 nil #'icl-browser))

(provide 'icl)
;;; icl.el ends here

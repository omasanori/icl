;;; inspector.lisp --- Interactive object inspector for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Debug Logging
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *inspector-debug* nil
  "When T, enable verbose inspector debug logging.")

(defvar *inspector-log-stream* *error-output*
  "Stream for inspector debug logging. Set at load time to the real terminal.")

(defun inspector-log (format-string &rest args)
  "Log a debug message if *inspector-debug* is enabled."
  (when *inspector-debug*
    (apply #'format *inspector-log-stream*
           (concatenate 'string "~&;; [INSPECTOR] " format-string "~%")
           args)
    (force-output *inspector-log-stream*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Inspector State
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct inspector-state
  "State for the interactive inspector."
  (object nil)                          ; Current object being inspected
  (entries nil :type list)              ; List of (label . value-form) pairs
  (selected 0 :type fixnum)             ; Currently selected index
  (scroll-offset 0 :type fixnum)        ; Scroll position
  (max-visible 15 :type fixnum)         ; Max items visible
  (path nil :type list)                 ; Navigation path (list of labels)
  (history nil :type list)              ; Stack of previous states for 'back'
  (title "" :type string))              ; Title for current view

(defvar *inspector* nil
  "Current inspector state.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Object Inspection via Slynk
;;; ─────────────────────────────────────────────────────────────────────────────

(defun slynk-inspect-object (form-string)
  "Inspect an object via Slynk. Returns (values data error-string).
   Uses backend-eval with slynk:init-inspector for thread-safe operation."
  (inspector-log "slynk-inspect-object called with form-string: ~S" form-string)
  (unless *slynk-connected-p*
    (inspector-log "slynk-inspect-object: NOT CONNECTED to backend server")
    (return-from slynk-inspect-object (values nil "Not connected to backend server")))
  (handler-case
      (let* ((code (format nil "(slynk:init-inspector ~S)" form-string)))
        (inspector-log "slynk-inspect-object: sending code to backend: ~S" code)
        (let* ((results (backend-eval code))
               (result-string (first results)))
          (inspector-log "slynk-inspect-object: backend-eval returned ~D result(s)" (length results))
          (inspector-log "slynk-inspect-object: result-string length=~A, preview=~S"
                         (if result-string (length result-string) "nil")
                         (if result-string (subseq result-string 0 (min 200 (length result-string))) nil))
          (let ((data (when result-string
                        (handler-case
                            (read-from-string result-string)
                          (error (e)
                            (inspector-log "slynk-inspect-object: ERROR parsing result: ~A" e)
                            nil)))))
            (inspector-log "slynk-inspect-object: parsed data type=~A, keys=~S"
                           (type-of data)
                           (when (listp data)
                             (loop for (k v) on data by #'cddr collect k)))
            (when data
              (inspector-log "slynk-inspect-object: :title=~S" (getf data :title))
              (inspector-log "slynk-inspect-object: :content type=~A length=~A"
                             (type-of (getf data :content))
                             (if (listp (getf data :content)) (length (getf data :content)) "N/A")))
            (values data nil))))
    (error (e)
      (inspector-log "slynk-inspect-object: EXCEPTION: ~A" e)
      (values nil (princ-to-string e)))))

(defun slynk-inspector-action (index)
  "Perform inspector action (drill down) at INDEX."
  (inspector-log "slynk-inspector-action called with index: ~D" index)
  (unless *slynk-connected-p*
    (inspector-log "slynk-inspector-action: NOT CONNECTED")
    (return-from slynk-inspector-action nil))
  (handler-case
      (let* ((code (format nil "(slynk:inspect-nth-part ~D)" index)))
        (inspector-log "slynk-inspector-action: sending code: ~S" code)
        (let ((result-string (first (backend-eval code))))
          (inspector-log "slynk-inspector-action: result-string length=~A"
                         (if result-string (length result-string) "nil"))
          (when result-string
            (let ((data (handler-case
                            (read-from-string result-string)
                          (error (e)
                            (inspector-log "slynk-inspector-action: ERROR parsing: ~A" e)
                            nil))))
              (inspector-log "slynk-inspector-action: parsed data type=~A" (type-of data))
              data))))
    (error (e)
      (inspector-log "slynk-inspector-action: EXCEPTION: ~A" e)
      nil)))

(defun slynk-inspector-pop ()
  "Go back in the Slynk inspector."
  (inspector-log "slynk-inspector-pop called")
  (unless *slynk-connected-p*
    (inspector-log "slynk-inspector-pop: NOT CONNECTED")
    (return-from slynk-inspector-pop nil))
  (handler-case
      (let ((result-string (first (backend-eval "(slynk:inspector-pop)"))))
        (inspector-log "slynk-inspector-pop: result-string length=~A"
                       (if result-string (length result-string) "nil"))
        (when result-string
          (let ((data (handler-case
                          (read-from-string result-string)
                        (error (e)
                          (inspector-log "slynk-inspector-pop: ERROR parsing: ~A" e)
                          nil))))
            (inspector-log "slynk-inspector-pop: parsed data type=~A" (type-of data))
            data)))
    (error (e)
      (inspector-log "slynk-inspector-pop: EXCEPTION: ~A" e)
      nil)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Parse Slynk Inspector Content
;;; ─────────────────────────────────────────────────────────────────────────────

(defun newline-string-p (str)
  "Return T if STR is just whitespace/newline."
  (and (stringp str)
       (plusp (length str))
       (find #\Newline str)))

(defun ensure-string (x)
  "Convert X to a string. If already a string, return it. Otherwise use PRINC-TO-STRING."
  (cond ((stringp x) x)
        ((null x) "")
        (t (princ-to-string x))))

(defun parse-inspector-content (content)
  "Parse Slynk inspector content into a list of entries.
   Each entry is (label value-string action-index) where action-index
   may be NIL for non-drillable items."
  (inspector-log "parse-inspector-content: input type=~A, length=~A"
                 (type-of content)
                 (if (listp content) (length content) "N/A"))
  (inspector-log "parse-inspector-content: first 5 items: ~S"
                 (if (listp content) (subseq content 0 (min 5 (length content))) content))
  (let ((entries nil)
        (current-label nil)
        (current-value nil)
        (current-action nil)
        (item-count 0))
    (dolist (item content)
      (incf item-count)
      (when (< item-count 20)  ; Log first 20 items in detail
        (inspector-log "parse-inspector-content: item[~D] type=~A value=~S"
                       item-count (type-of item)
                       (if (stringp item)
                           (subseq item 0 (min 50 (length item)))
                           item)))
      (cond
        ;; Newline string - end of current entry
        ((newline-string-p item)
         (when (or current-label current-value)
           (let ((entry (list (string-trim '(#\Space #\Tab #\: ) (ensure-string current-label))
                              (string-trim '(#\Space #\Tab) (ensure-string current-value))
                              current-action)))
             (inspector-log "parse-inspector-content: pushing entry: ~S" entry)
             (push entry entries)))
         (setf current-label nil
               current-value nil
               current-action nil))
        ;; Regular string - part of label or value
        ((stringp item)
         (if current-value
             (setf current-value (concatenate 'string (ensure-string current-value) item))
             (if current-label
                 (setf current-label (concatenate 'string (ensure-string current-label) item))
                 (setf current-label item))))
        ;; (:value "string" action-id) - a drillable value
        ((and (listp item) (eql (first item) :value))
         (inspector-log "parse-inspector-content: found :value item, second=~S third=~S"
                        (second item) (third item))
         (setf current-value (ensure-string (second item)))
         (when (third item)
           (setf current-action (third item))))
        ;; (:action "label" action-id) - an action button
        ((and (listp item) (eql (first item) :action))
         (inspector-log "parse-inspector-content: found :action item, second=~S third=~S"
                        (second item) (third item))
         (setf current-label (ensure-string (second item)))
         (setf current-action (third item)))
        ;; Unknown item type
        (t
         (inspector-log "parse-inspector-content: UNKNOWN item type: ~A value=~S"
                        (type-of item) item))))
    ;; Don't forget last entry if no trailing newline
    (when (or current-label current-value)
      (let ((entry (list (string-trim '(#\Space #\Tab #\:) (ensure-string current-label))
                         (string-trim '(#\Space #\Tab) (ensure-string current-value))
                         current-action)))
        (inspector-log "parse-inspector-content: pushing final entry: ~S" entry)
        (push entry entries)))
    (let ((result (nreverse entries)))
      (inspector-log "parse-inspector-content: returning ~D entries" (length result))
      (when (plusp (length result))
        (inspector-log "parse-inspector-content: first entry: ~S" (first result))
        (inspector-log "parse-inspector-content: last entry: ~S" (car (last result))))
      result)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Inspector Rendering
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *inspector-width* 70
  "Width of inspector display.")

(defvar *inspector-lines-drawn* 0
  "Number of lines drawn by last render, for cleanup.")

(defun render-inspector (&optional first-render)
  "Render the current inspector state."
  (let* ((state *inspector*)
         (entries (inspector-state-entries state))
         (selected (inspector-state-selected state))
         (scroll (inspector-state-scroll-offset state))
         (max-visible (inspector-state-max-visible state))
         (title (inspector-state-title state))
         (path (inspector-state-path state))
         (total (length entries))
         (visible-count (min max-visible (- total scroll)))
         (width *inspector-width*)
         (lines-to-draw 0))
    ;; If not first render, move cursor back up to overwrite previous render
    (unless first-render
      (when (> *inspector-lines-drawn* 0)
        (format t "~C[~DA" #\Escape *inspector-lines-drawn*)))
    ;; Draw top border with title (clear line first)
    (format t "~C[2K" #\Escape)  ; Clear line
    (let ((display-title (truncate-string title (- width 6))))
      (format t "~A┌─ ~A ~A┐~A~%"
              *ansi-bold*
              display-title
              (make-string (max 0 (- width 5 (length display-title))) :initial-element #\─)
              *ansi-reset*))
    (incf lines-to-draw)
    ;; Draw entries
    (dotimes (i visible-count)
      (format t "~C[2K" #\Escape)  ; Clear line
      (let* ((idx (+ scroll i))
             (entry (nth idx entries))
             (label (first entry))
             (value (second entry))
             (action (third entry))
             (is-selected (= idx selected))
             (drillable action)
             ;; Format the line
             (prefix (if is-selected " > " "   "))
             (suffix (if drillable " →" "  "))
             (content (format nil "~A~A"
                              (if (string= label "") "" (format nil "~A: " label))
                              value))
             (truncated (truncate-string content (- width 8))))
        (if is-selected
            (format t "│~A~A~A~A~VA~A│~%"
                    *ansi-reverse* prefix truncated suffix
                    (max 0 (- width 7 (length truncated))) ""
                    *ansi-reset*)
            (format t "│~A~A~A~VA│~%"
                    prefix truncated suffix
                    (max 0 (- width 7 (length truncated))) "")))
      (incf lines-to-draw))
    ;; Pad remaining lines if needed
    (dotimes (i (- max-visible visible-count))
      (format t "~C[2K│~VA│~%" #\Escape (- width 2) "")
      (incf lines-to-draw))
    ;; Draw bottom border
    (format t "~C[2K└~A┘~%" #\Escape (make-string (- width 2) :initial-element #\─))
    (incf lines-to-draw)
    ;; Draw path if we've navigated
    (when path
      (format t "~C[2K~A Path: ~{~A~^ → ~}~A~%"
              #\Escape *ansi-dim* (reverse path) *ansi-reset*)
      (incf lines-to-draw))
    ;; Draw help line
    (format t "~C[2K~A [↑/↓] navigate  [Enter] drill in  [b] back  [q] quit~A~%"
            #\Escape *ansi-dim* *ansi-reset*)
    (incf lines-to-draw)
    ;; Show position if scrollable
    (when (> total max-visible)
      (format t "~C[2K~A [~D/~D]~A~%" #\Escape *ansi-dim* (1+ selected) total *ansi-reset*)
      (incf lines-to-draw))
    ;; Clear any leftover lines from previous render (if we drew fewer lines this time)
    (when (> *inspector-lines-drawn* lines-to-draw)
      (dotimes (i (- *inspector-lines-drawn* lines-to-draw))
        (format t "~C[2K~%" #\Escape)))
    ;; Remember how many lines we drew
    (setf *inspector-lines-drawn* lines-to-draw)
    (force-output)))

(defun truncate-string (str max-len)
  "Truncate STR to MAX-LEN, adding ... if needed."
  (if (<= (length str) max-len)
      str
      (concatenate 'string (subseq str 0 (- max-len 3)) "...")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Inspector Navigation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun inspector-move (direction)
  "Move selection in DIRECTION (:up or :down)."
  (let* ((state *inspector*)
         (entries (inspector-state-entries state))
         (total (length entries))
         (selected (inspector-state-selected state))
         (max-visible (inspector-state-max-visible state))
         (scroll (inspector-state-scroll-offset state)))
    (when (zerop total) (return-from inspector-move))
    (let ((new-selected (case direction
                          (:up (max 0 (1- selected)))
                          (:down (min (1- total) (1+ selected)))
                          (otherwise selected))))
      (setf (inspector-state-selected state) new-selected)
      ;; Adjust scroll
      (when (< new-selected scroll)
        (setf (inspector-state-scroll-offset state) new-selected))
      (when (>= new-selected (+ scroll max-visible))
        (setf (inspector-state-scroll-offset state)
              (- new-selected max-visible -1))))))

(defun inspector-drill-down ()
  "Drill into the selected entry if it's drillable."
  (let* ((state *inspector*)
         (entries (inspector-state-entries state))
         (selected (inspector-state-selected state))
         (entry (nth selected entries))
         (action (third entry)))
    (when action
      (let ((new-data (slynk-inspector-action action)))
        (when new-data
          ;; Save current label for path
          (let ((label (first entry)))
            (push label (inspector-state-path state)))
          ;; Update state with new data
          (update-inspector-state new-data))))))

(defun inspector-go-back ()
  "Go back to previous inspection level."
  (let ((state *inspector*))
    (when (inspector-state-path state)
      (let ((new-data (slynk-inspector-pop)))
        (when new-data
          (pop (inspector-state-path state))
          (update-inspector-state new-data))))))

(defun update-inspector-state (data)
  "Update inspector state with new data from Slynk."
  (let* ((state *inspector*)
         (raw-content (getf data :content))
         ;; Content is (list-of-items . metadata), extract first element
         (content (if (and (listp raw-content) (listp (first raw-content)))
                      (first raw-content)
                      raw-content)))
    (setf (inspector-state-title state) (or (getf data :title) "Object")
          (inspector-state-entries state) (parse-inspector-content content)
          (inspector-state-selected state) 0
          (inspector-state-scroll-offset state) 0)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Inspector Input Loop
;;; ─────────────────────────────────────────────────────────────────────────────

(defun clear-inspector ()
  "Clear the inspector display."
  (when (> *inspector-lines-drawn* 0)
    ;; Move up to start of inspector, go to column 1, clear line and everything below
    (format t "~C[~DA~C[G~C[2K~C[J"
            #\Escape *inspector-lines-drawn* #\Escape #\Escape #\Escape)
    (setf *inspector-lines-drawn* 0)
    (force-output)))

(defun run-inspector (form-string)
  "Run the interactive inspector on FORM-STRING."
  (let ((data (slynk-inspect-object form-string)))
    (unless data
      (format t "~&Could not inspect object.~%")
      (return-from run-inspector nil))
    ;; Initialize inspector state
    (setf *inspector* (make-inspector-state))
    (setf *inspector-lines-drawn* 0)
    (update-inspector-state data)
    ;; Enter raw mode for keyboard input
    (let ((first-render t))
      (with-raw-mode
        (loop
          (render-inspector first-render)
          (setf first-render nil)
          (let ((key (read-key)))
            (cond
              ;; Quit
              ((or (eql key #\q) (eql key #\Q) (eql key :eof))
               (return))
              ;; Back
              ((or (eql key #\b) (eql key #\B) (eql key :backspace) (eql key :escape))
               (if (inspector-state-path *inspector*)
                   (inspector-go-back)
                   (return)))  ; At top level, back exits
              ;; Enter - drill down
              ((eql key :enter)
               (inspector-drill-down))
              ;; Arrow keys
              ((eql key :up)
               (inspector-move :up))
              ((eql key :down)
               (inspector-move :down))
              ;; Vim-style navigation
              ((or (eql key #\k) (eql key #\K))
               (inspector-move :up))
              ((or (eql key #\j) (eql key #\J))
               (inspector-move :down)))))))
    ;; Clear inspector on exit
    (clear-inspector)))

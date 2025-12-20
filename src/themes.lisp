;;; themes.lisp --- Theming system for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Terminal Theme Structure
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct terminal-theme
  "Theme definition for terminal/TUI interface.
   Colors are 256-color ANSI codes (0-255)."
  (name nil :type (or keyword null))
  (display-name "" :type string)
  (dark-p t :type boolean)
  ;; Syntax highlighting colors (256-color codes)
  (hl-keyword 205 :type fixnum)      ; Keywords like DEFUN, LET
  (hl-string 114 :type fixnum)       ; String literals
  (hl-comment 245 :type fixnum)      ; Comments
  (hl-number 209 :type fixnum)       ; Numeric literals
  (hl-symbol 252 :type fixnum)       ; Regular symbols
  (hl-package 81 :type fixnum)       ; Package prefixes
  (hl-special 141 :type fixnum)      ; Special forms
  ;; UI colors
  (prompt-primary 75 :type fixnum)   ; Main prompt color
  (prompt-package 243 :type fixnum)  ; Package name in prompt
  (error 196 :type fixnum)           ; Error messages
  (warning 214 :type fixnum)         ; Warnings
  (info 75 :type fixnum)             ; Info messages
  (dim 243 :type fixnum))            ; Dimmed/secondary text

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Browser Theme Structure
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct browser-theme
  "Theme definition for browser interface.
   Colors are CSS hex values (strings)."
  (name nil :type (or keyword null))
  (display-name "" :type string)
  (dark-p t :type boolean)
  ;; Main UI colors
  (bg-primary "#1e1e2e" :type string)
  (bg-secondary "#313244" :type string)
  (bg-tertiary "#45475a" :type string)
  (fg-primary "#cdd6f4" :type string)
  (fg-secondary "#bac2de" :type string)
  (fg-muted "#6c7086" :type string)
  (accent "#89b4fa" :type string)
  (accent-hover "#b4befe" :type string)
  (border "#45475a" :type string)
  ;; Syntax highlighting
  (syntax-keyword "#cba6f7" :type string)
  (syntax-string "#a6e3a1" :type string)
  (syntax-comment "#6c7086" :type string)
  (syntax-number "#fab387" :type string)
  (syntax-symbol "#cdd6f4" :type string)
  (syntax-package "#89dceb" :type string)
  (syntax-special "#f5c2e7" :type string)
  (syntax-error "#f38ba8" :type string)
  ;; xterm.js ANSI palette (16 colors)
  (ansi-black "#45475a" :type string)
  (ansi-red "#f38ba8" :type string)
  (ansi-green "#a6e3a1" :type string)
  (ansi-yellow "#f9e2af" :type string)
  (ansi-blue "#89b4fa" :type string)
  (ansi-magenta "#cba6f7" :type string)
  (ansi-cyan "#89dceb" :type string)
  (ansi-white "#bac2de" :type string)
  (ansi-bright-black "#585b70" :type string)
  (ansi-bright-red "#f38ba8" :type string)
  (ansi-bright-green "#a6e3a1" :type string)
  (ansi-bright-yellow "#f9e2af" :type string)
  (ansi-bright-blue "#89b4fa" :type string)
  (ansi-bright-magenta "#cba6f7" :type string)
  (ansi-bright-cyan "#89dceb" :type string)
  (ansi-bright-white "#a6adc8" :type string))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Theme Registries
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *terminal-themes* (make-hash-table :test #'eq)
  "Registry of terminal themes by name (keyword).")

(defvar *browser-themes* (make-hash-table :test #'eq)
  "Registry of browser themes by name (keyword).")

(defvar *current-terminal-theme* nil
  "Currently active terminal theme.")

(defvar *current-browser-theme* nil
  "Currently active browser theme.")

(defvar *default-dark-terminal-theme* :dracula
  "Default terminal theme for dark mode.")

(defvar *default-light-terminal-theme* :github-light
  "Default terminal theme for light mode.")

(defvar *default-dark-browser-theme* :dracula
  "Default browser theme for dark mode.")

(defvar *default-light-browser-theme* :github-light
  "Default browser theme for light mode.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Theme Registration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun register-terminal-theme (theme)
  "Register a terminal theme."
  (setf (gethash (terminal-theme-name theme) *terminal-themes*) theme))

(defun register-browser-theme (theme)
  "Register a browser theme."
  (setf (gethash (browser-theme-name theme) *browser-themes*) theme))

(defun find-terminal-theme (name)
  "Find a terminal theme by name (keyword)."
  (gethash name *terminal-themes*))

(defun find-browser-theme (name)
  "Find a browser theme by name (keyword)."
  (gethash name *browser-themes*))

(defun list-terminal-themes ()
  "List all registered terminal themes."
  (let ((themes nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v themes))
             *terminal-themes*)
    (sort themes #'string< :key #'terminal-theme-display-name)))

(defun list-browser-themes ()
  "List all registered browser themes."
  (let ((themes nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v themes))
             *browser-themes*)
    (sort themes #'string< :key #'browser-theme-display-name)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Dark Mode Detection
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *terminal-dark-mode-override* nil
  "User override for terminal dark mode detection.
   Set to T for dark, NIL for light, or :auto for auto-detection.")

(defun query-terminal-background ()
  "Try to query the terminal's background color using OSC 11.
   Returns :dark, :light, or NIL if unable to detect."
  (ignore-errors
    (with-raw-mode
      ;; Send OSC 11 query: request background color
      (format t "~C]11;?~C\\" #\Escape #\Escape)
      (force-output)
      ;; Give terminal time to respond
      (sleep 0.05)
      ;; Try to read response (format: ESC ] 11 ; rgb:RRRR/GGGG/BBBB ESC \)
      (when (listen)
        (let ((response (make-string-output-stream)))
          (loop for i from 0 below 50
                while (listen)
                do (write-char (read-char) response))
          (let ((str (get-output-stream-string response)))
            ;; Parse rgb:RRRR/GGGG/BBBB format
            (when (search "rgb:" str)
              (let* ((rgb-start (+ 4 (search "rgb:" str)))
                     (rgb-part (subseq str rgb-start (min (+ rgb-start 20) (length str))))
                     (parts (uiop:split-string rgb-part :separator "/")))
                (when (>= (length parts) 3)
                  ;; Get first 2 hex digits of each component (0-255 range)
                  (let ((r (ignore-errors (parse-integer (subseq (first parts) 0 2) :radix 16)))
                        (g (ignore-errors (parse-integer (subseq (second parts) 0 2) :radix 16)))
                        (b (ignore-errors (parse-integer (subseq (third parts) 0 2) :radix 16))))
                    (when (and r g b)
                      ;; Calculate luminance: dark if < 128
                      (let ((luminance (/ (+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) 1.0)))
                        (if (< luminance 128) :dark :light)))))))))))))

(defun detect-terminal-dark-mode ()
  "Detect if the terminal prefers dark mode.
   Returns T for dark mode, NIL for light mode."
  ;; Check user override first
  (when (and (boundp '*terminal-dark-mode-override*)
             (not (eq *terminal-dark-mode-override* :auto)))
    (return-from detect-terminal-dark-mode *terminal-dark-mode-override*))
  ;; Check COLORFGBG (format: 'foreground;background')
  ;; Background >= 8 typically means light (white is 15)
  (let ((colorfgbg (uiop:getenv "COLORFGBG")))
    (when (and colorfgbg (plusp (length colorfgbg)))
      (let ((parts (uiop:split-string colorfgbg :separator ";")))
        (when (>= (length parts) 2)
          (let ((bg (ignore-errors (parse-integer (second parts)))))
            (when bg
              (return-from detect-terminal-dark-mode (< bg 8))))))))
  ;; Check for known dark-default terminals
  (let ((term-program (uiop:getenv "TERM_PROGRAM")))
    (when (and term-program (plusp (length term-program)))
      (when (member term-program '("iTerm.app" "Hyper" "Alacritty" "kitty" "WezTerm")
                    :test #'string-equal)
        (return-from detect-terminal-dark-mode t))))
  ;; Try to query terminal directly (works in xterm, gnome-terminal, etc.)
  (let ((queried (query-terminal-background)))
    (when queried
      (return-from detect-terminal-dark-mode (eq queried :dark))))
  ;; Check GNOME/GTK dark mode preference
  (let ((gtk-theme (uiop:getenv "GTK_THEME")))
    (when (and gtk-theme (search "dark" gtk-theme :test #'char-equal))
      (return-from detect-terminal-dark-mode t)))
  ;; Default to light mode when we can't detect
  ;; (safer since dark text on white is more common than expected)
  nil)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Theme Application
;;; ─────────────────────────────────────────────────────────────────────────────

(defun apply-terminal-theme (theme)
  "Apply a terminal theme, updating all color variables."
  (when (keywordp theme)
    (setf theme (find-terminal-theme theme)))
  (unless theme
    (error "Terminal theme not found"))
  (setf *current-terminal-theme* theme)
  ;; Update syntax highlighting colors
  (setf *hl-keyword-color* (terminal-theme-hl-keyword theme)
        *hl-string-color* (terminal-theme-hl-string theme)
        *hl-comment-color* (terminal-theme-hl-comment theme)
        *hl-number-color* (terminal-theme-hl-number theme)
        *hl-symbol-color* (terminal-theme-hl-symbol theme)
        *hl-package-color* (terminal-theme-hl-package theme)
        *hl-special-color* (terminal-theme-hl-special theme))
  ;; Update UI colors
  (setf *color-prompt* (terminal-theme-prompt-primary theme)
        *color-package* (terminal-theme-prompt-package theme)
        *color-error* (terminal-theme-error theme)
        *color-warning* (terminal-theme-warning theme)
        *color-info* (terminal-theme-info theme)
        *color-dim* (terminal-theme-dim theme))
  ;; Regenerate ANSI escape sequences
  (refresh-ansi-codes)
  theme)

(defun apply-browser-theme (theme)
  "Apply a browser theme, notifying all connected clients."
  (when (keywordp theme)
    (setf theme (find-browser-theme theme)))
  (unless theme
    (error "Browser theme not found"))
  (setf *current-browser-theme* theme)
  ;; Broadcast to all connected WebSocket clients
  (broadcast-browser-theme theme)
  theme)

(defun refresh-ansi-codes ()
  "Regenerate ANSI escape code strings from current colors."
  (setf *ansi-prompt* (format nil "~C[38;5;~Dm" #\Escape *color-prompt*)
        *ansi-package* (format nil "~C[38;5;~Dm" #\Escape *color-package*)
        *ansi-error* (format nil "~C[38;5;~Dm" #\Escape *color-error*)
        *ansi-warning* (format nil "~C[38;5;~Dm" #\Escape *color-warning*)
        *ansi-info* (format nil "~C[38;5;~Dm" #\Escape *color-info*)
        *ansi-fg-gray* (format nil "~C[38;5;~Dm" #\Escape *color-dim*))
  ;; Also refresh syntax highlighting colors if that function is defined
  (when (fboundp 'refresh-highlight-colors)
    (funcall 'refresh-highlight-colors)))

(defun broadcast-browser-theme (theme)
  "Broadcast theme to all connected browser clients.
   This is a no-op if the browser module isn't loaded."
  (declare (ignore theme))
  ;; Will be replaced by browser.lisp
  nil)

(defun auto-select-terminal-theme ()
  "Auto-select terminal theme based on dark mode detection."
  (if (detect-terminal-dark-mode)
      (apply-terminal-theme *default-dark-terminal-theme*)
      (apply-terminal-theme *default-light-terminal-theme*)))

(defun auto-select-browser-theme (&optional dark-p)
  "Auto-select browser theme. If DARK-P is provided, use that preference."
  (let ((dark (if (null dark-p)
                  (detect-terminal-dark-mode)  ; Fallback to terminal detection
                  dark-p)))
    (if dark
        (apply-browser-theme *default-dark-browser-theme*)
        (apply-browser-theme *default-light-browser-theme*))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Browser Theme CSS Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun browser-theme-to-css (theme)
  "Generate CSS variable declarations from a browser theme.
   Includes both application variables and Dockview theming variables."
  (let ((bg-primary (browser-theme-bg-primary theme))
        (bg-secondary (browser-theme-bg-secondary theme))
        (bg-tertiary (browser-theme-bg-tertiary theme))
        (fg-primary (browser-theme-fg-primary theme))
        (fg-secondary (browser-theme-fg-secondary theme))
        (fg-muted (browser-theme-fg-muted theme))
        (accent (browser-theme-accent theme))
        (accent-hover (browser-theme-accent-hover theme))
        (border (browser-theme-border theme)))
    (format nil ":root {
  /* Application theme variables */
  --bg-primary: ~A;
  --bg-secondary: ~A;
  --bg-tertiary: ~A;
  --fg-primary: ~A;
  --fg-secondary: ~A;
  --fg-muted: ~A;
  --accent: ~A;
  --accent-hover: ~A;
  --border: ~A;
  --syntax-keyword: ~A;
  --syntax-string: ~A;
  --syntax-comment: ~A;
  --syntax-number: ~A;
  --syntax-symbol: ~A;
  --syntax-package: ~A;
  --syntax-special: ~A;
  --syntax-error: ~A;
}

/* Dockview theming - CSS variables */
#layout-container.icl-dockview-theme {
  --dv-paneview-active-outline-color: ~A;
  --dv-group-view-background-color: ~A;
  --dv-tabs-and-actions-container-background-color: ~A;
  --dv-activegroup-visiblepanel-tab-background-color: ~A;
  --dv-activegroup-hiddenpanel-tab-background-color: ~A;
  --dv-activegroup-visiblepanel-tab-color: ~A;
  --dv-activegroup-hiddenpanel-tab-color: ~A;
  --dv-inactivegroup-visiblepanel-tab-background-color: ~A;
  --dv-inactivegroup-hiddenpanel-tab-background-color: ~A;
  --dv-inactivegroup-visiblepanel-tab-color: ~A;
  --dv-inactivegroup-hiddenpanel-tab-color: ~A;
  --dv-tab-divider-color: ~A;
  --dv-separator-border: ~A;
  --dv-paneview-header-border-color: ~A;
  --dv-tabs-container-scrollbar-color: ~A;
  --dv-icon-hover-background-color: ~A;
  --dv-drag-over-background-color: ~A;
  --dv-drag-over-border-color: ~A;
}

/* Dockview explicit element styling for better compatibility */
.icl-dockview-theme .dv-tabs-and-actions-container {
  background-color: ~A !important;
}
.icl-dockview-theme .dv-tab {
  color: ~A !important;
}
.icl-dockview-theme .dv-tab.dv-active-tab {
  background-color: ~A !important;
  color: ~A !important;
}
.icl-dockview-theme .dv-tab:not(.dv-active-tab) {
  background-color: ~A !important;
}
.icl-dockview-theme .dv-groupview {
  background-color: ~A !important;
}
.icl-dockview-theme .dv-resize-handle-border,
.icl-dockview-theme .dv-tabs-and-actions-container {
  border-color: ~A !important;
}
.icl-dockview-theme .dv-default-tab-action svg {
  fill: ~A !important;
}"
            ;; Application variables
            bg-primary bg-secondary bg-tertiary
            fg-primary fg-secondary fg-muted
            accent accent-hover border
            (browser-theme-syntax-keyword theme)
            (browser-theme-syntax-string theme)
            (browser-theme-syntax-comment theme)
            (browser-theme-syntax-number theme)
            (browser-theme-syntax-symbol theme)
            (browser-theme-syntax-package theme)
            (browser-theme-syntax-special theme)
            (browser-theme-syntax-error theme)
            ;; Dockview CSS variables
            accent                          ; paneview active outline
            bg-primary                      ; group view background
            bg-secondary                    ; tabs container background
            bg-tertiary                     ; active group visible tab bg
            bg-secondary                    ; active group hidden tab bg
            fg-primary                      ; active group visible tab text
            fg-muted                        ; active group hidden tab text
            bg-secondary                    ; inactive group visible tab bg
            bg-secondary                    ; inactive group hidden tab bg
            fg-secondary                    ; inactive group visible tab text
            fg-muted                        ; inactive group hidden tab text
            border                          ; tab divider
            border                          ; separator border
            border                          ; paneview header border
            fg-muted                        ; scrollbar color
            bg-tertiary                     ; icon hover background
            (format nil "~Aaa" accent)      ; drag over bg (with alpha)
            accent                          ; drag over border
            ;; Explicit element styling
            bg-secondary                    ; tabs container bg
            fg-secondary                    ; tab text color
            bg-tertiary                     ; active tab bg
            fg-primary                      ; active tab text
            bg-secondary                    ; inactive tab bg
            bg-primary                      ; groupview bg
            border                          ; borders
            fg-muted)))

(defun browser-theme-to-xterm-options (theme)
  "Generate xterm.js theme options as an alist."
  (list (cons "background" (browser-theme-bg-primary theme))
        (cons "foreground" (browser-theme-fg-primary theme))
        (cons "cursor" (browser-theme-accent theme))
        (cons "cursorAccent" (browser-theme-bg-primary theme))
        (cons "selection" (browser-theme-bg-tertiary theme))
        (cons "black" (browser-theme-ansi-black theme))
        (cons "red" (browser-theme-ansi-red theme))
        (cons "green" (browser-theme-ansi-green theme))
        (cons "yellow" (browser-theme-ansi-yellow theme))
        (cons "blue" (browser-theme-ansi-blue theme))
        (cons "magenta" (browser-theme-ansi-magenta theme))
        (cons "cyan" (browser-theme-ansi-cyan theme))
        (cons "white" (browser-theme-ansi-white theme))
        (cons "brightBlack" (browser-theme-ansi-bright-black theme))
        (cons "brightRed" (browser-theme-ansi-bright-red theme))
        (cons "brightGreen" (browser-theme-ansi-bright-green theme))
        (cons "brightYellow" (browser-theme-ansi-bright-yellow theme))
        (cons "brightBlue" (browser-theme-ansi-bright-blue theme))
        (cons "brightMagenta" (browser-theme-ansi-bright-magenta theme))
        (cons "brightCyan" (browser-theme-ansi-bright-cyan theme))
        (cons "brightWhite" (browser-theme-ansi-bright-white theme))))

(defun alist-to-hash (alist)
  "Convert an alist to a hash table for JSON serialization."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist ht)
      (setf (gethash (car pair) ht)
            (if (and (consp (cdr pair)) (consp (car (cdr pair))))
                ;; Nested alist
                (alist-to-hash (cdr pair))
                (cdr pair))))))

(defun browser-theme-to-json (theme)
  "Convert browser theme to JSON for WebSocket transmission."
  (let* ((xterm-alist (browser-theme-to-xterm-options theme))
         (xterm-hash (alist-to-hash xterm-alist))
         (data (make-hash-table :test 'equal)))
    (setf (gethash "name" data) (string-downcase (symbol-name (browser-theme-name theme)))
          (gethash "displayName" data) (browser-theme-display-name theme)
          (gethash "darkP" data) (browser-theme-dark-p theme)
          (gethash "css" data) (browser-theme-to-css theme)
          (gethash "xterm" data) xterm-hash
          ;; Use empty class - our CSS variables will control all styling
          (gethash "dockviewTheme" data) "icl-dockview-theme")
    (com.inuoe.jzon:stringify data)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Custom Theme Definition Macros
;;; ─────────────────────────────────────────────────────────────────────────────

(defmacro define-terminal-theme (name &rest args)
  "Define and register a custom terminal theme.
   NAME should be a keyword.
   ARGS are keyword arguments matching terminal-theme slots."
  `(register-terminal-theme
    (make-terminal-theme :name ,name ,@args)))

(defmacro define-browser-theme (name &rest args)
  "Define and register a custom browser theme.
   NAME should be a keyword.
   ARGS are keyword arguments matching browser-theme slots."
  `(register-browser-theme
    (make-browser-theme :name ,name ,@args)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: DRACULA
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :dracula
  :display-name "Dracula"
  :dark-p t
  :hl-keyword 212    ; Pink
  :hl-string 84      ; Green
  :hl-comment 103    ; Comment gray
  :hl-number 215     ; Orange
  :hl-symbol 231     ; Foreground
  :hl-package 117    ; Cyan
  :hl-special 141    ; Purple
  :prompt-primary 141
  :prompt-package 103
  :error 210
  :warning 215
  :info 117
  :dim 103))

(register-browser-theme
 (make-browser-theme
  :name :dracula
  :display-name "Dracula"
  :dark-p t
  :bg-primary "#282a36"
  :bg-secondary "#44475a"
  :bg-tertiary "#6272a4"
  :fg-primary "#f8f8f2"
  :fg-secondary "#f8f8f2"
  :fg-muted "#6272a4"
  :accent "#bd93f9"
  :accent-hover "#ff79c6"
  :border "#44475a"
  :syntax-keyword "#ff79c6"
  :syntax-string "#50fa7b"
  :syntax-comment "#6272a4"
  :syntax-number "#ffb86c"
  :syntax-symbol "#f8f8f2"
  :syntax-package "#8be9fd"
  :syntax-special "#bd93f9"
  :syntax-error "#ff5555"
  :ansi-black "#21222c"
  :ansi-red "#ff5555"
  :ansi-green "#50fa7b"
  :ansi-yellow "#f1fa8c"
  :ansi-blue "#bd93f9"
  :ansi-magenta "#ff79c6"
  :ansi-cyan "#8be9fd"
  :ansi-white "#f8f8f2"
  :ansi-bright-black "#6272a4"
  :ansi-bright-red "#ff6e6e"
  :ansi-bright-green "#69ff94"
  :ansi-bright-yellow "#ffffa5"
  :ansi-bright-blue "#d6acff"
  :ansi-bright-magenta "#ff92df"
  :ansi-bright-cyan "#a4ffff"
  :ansi-bright-white "#ffffff"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: NORD
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :nord
  :display-name "Nord"
  :dark-p t
  :hl-keyword 176    ; Purple
  :hl-string 150     ; Green
  :hl-comment 60     ; Comment
  :hl-number 215     ; Orange
  :hl-symbol 255     ; Snow
  :hl-package 110    ; Frost
  :hl-special 176    ; Purple
  :prompt-primary 110
  :prompt-package 60
  :error 174
  :warning 215
  :info 110
  :dim 60))

(register-browser-theme
 (make-browser-theme
  :name :nord
  :display-name "Nord"
  :dark-p t
  :bg-primary "#2e3440"
  :bg-secondary "#3b4252"
  :bg-tertiary "#434c5e"
  :fg-primary "#eceff4"
  :fg-secondary "#e5e9f0"
  :fg-muted "#4c566a"
  :accent "#88c0d0"
  :accent-hover "#81a1c1"
  :border "#434c5e"
  :syntax-keyword "#b48ead"
  :syntax-string "#a3be8c"
  :syntax-comment "#616e88"
  :syntax-number "#d08770"
  :syntax-symbol "#eceff4"
  :syntax-package "#88c0d0"
  :syntax-special "#b48ead"
  :syntax-error "#bf616a"
  :ansi-black "#3b4252"
  :ansi-red "#bf616a"
  :ansi-green "#a3be8c"
  :ansi-yellow "#ebcb8b"
  :ansi-blue "#81a1c1"
  :ansi-magenta "#b48ead"
  :ansi-cyan "#88c0d0"
  :ansi-white "#e5e9f0"
  :ansi-bright-black "#4c566a"
  :ansi-bright-red "#bf616a"
  :ansi-bright-green "#a3be8c"
  :ansi-bright-yellow "#ebcb8b"
  :ansi-bright-blue "#81a1c1"
  :ansi-bright-magenta "#b48ead"
  :ansi-bright-cyan "#8fbcbb"
  :ansi-bright-white "#eceff4"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: ONE DARK
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :one-dark
  :display-name "One Dark"
  :dark-p t
  :hl-keyword 176    ; Purple
  :hl-string 114     ; Green
  :hl-comment 102    ; Comment
  :hl-number 173     ; Orange
  :hl-symbol 252     ; Foreground
  :hl-package 39     ; Cyan
  :hl-special 176    ; Purple
  :prompt-primary 39
  :prompt-package 102
  :error 204
  :warning 173
  :info 39
  :dim 102))

(register-browser-theme
 (make-browser-theme
  :name :one-dark
  :display-name "One Dark"
  :dark-p t
  :bg-primary "#282c34"
  :bg-secondary "#21252b"
  :bg-tertiary "#2c313a"
  :fg-primary "#abb2bf"
  :fg-secondary "#abb2bf"
  :fg-muted "#5c6370"
  :accent "#61afef"
  :accent-hover "#528bff"
  :border "#181a1f"
  :syntax-keyword "#c678dd"
  :syntax-string "#98c379"
  :syntax-comment "#5c6370"
  :syntax-number "#d19a66"
  :syntax-symbol "#abb2bf"
  :syntax-package "#56b6c2"
  :syntax-special "#c678dd"
  :syntax-error "#e06c75"
  :ansi-black "#282c34"
  :ansi-red "#e06c75"
  :ansi-green "#98c379"
  :ansi-yellow "#e5c07b"
  :ansi-blue "#61afef"
  :ansi-magenta "#c678dd"
  :ansi-cyan "#56b6c2"
  :ansi-white "#abb2bf"
  :ansi-bright-black "#5c6370"
  :ansi-bright-red "#e06c75"
  :ansi-bright-green "#98c379"
  :ansi-bright-yellow "#e5c07b"
  :ansi-bright-blue "#61afef"
  :ansi-bright-magenta "#c678dd"
  :ansi-bright-cyan "#56b6c2"
  :ansi-bright-white "#ffffff"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: GRUVBOX DARK
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :gruvbox-dark
  :display-name "Gruvbox Dark"
  :dark-p t
  :hl-keyword 175    ; Purple
  :hl-string 142     ; Green
  :hl-comment 102    ; Gray
  :hl-number 208     ; Orange
  :hl-symbol 223     ; Fg
  :hl-package 109    ; Blue
  :hl-special 175    ; Purple
  :prompt-primary 214
  :prompt-package 102
  :error 167
  :warning 214
  :info 109
  :dim 102))

(register-browser-theme
 (make-browser-theme
  :name :gruvbox-dark
  :display-name "Gruvbox Dark"
  :dark-p t
  :bg-primary "#282828"
  :bg-secondary "#3c3836"
  :bg-tertiary "#504945"
  :fg-primary "#ebdbb2"
  :fg-secondary "#d5c4a1"
  :fg-muted "#928374"
  :accent "#fe8019"
  :accent-hover "#fabd2f"
  :border "#504945"
  :syntax-keyword "#d3869b"
  :syntax-string "#b8bb26"
  :syntax-comment "#928374"
  :syntax-number "#fe8019"
  :syntax-symbol "#ebdbb2"
  :syntax-package "#83a598"
  :syntax-special "#d3869b"
  :syntax-error "#fb4934"
  :ansi-black "#282828"
  :ansi-red "#cc241d"
  :ansi-green "#98971a"
  :ansi-yellow "#d79921"
  :ansi-blue "#458588"
  :ansi-magenta "#b16286"
  :ansi-cyan "#689d6a"
  :ansi-white "#a89984"
  :ansi-bright-black "#928374"
  :ansi-bright-red "#fb4934"
  :ansi-bright-green "#b8bb26"
  :ansi-bright-yellow "#fabd2f"
  :ansi-bright-blue "#83a598"
  :ansi-bright-magenta "#d3869b"
  :ansi-bright-cyan "#8ec07c"
  :ansi-bright-white "#ebdbb2"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: TOKYO NIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :tokyo-night
  :display-name "Tokyo Night"
  :dark-p t
  :hl-keyword 177    ; Magenta
  :hl-string 150     ; Green
  :hl-comment 60     ; Comment
  :hl-number 215     ; Orange
  :hl-symbol 189     ; Foreground
  :hl-package 117    ; Cyan
  :hl-special 177    ; Magenta
  :prompt-primary 75
  :prompt-package 60
  :error 204
  :warning 215
  :info 117
  :dim 60))

(register-browser-theme
 (make-browser-theme
  :name :tokyo-night
  :display-name "Tokyo Night"
  :dark-p t
  :bg-primary "#1a1b26"
  :bg-secondary "#24283b"
  :bg-tertiary "#414868"
  :fg-primary "#c0caf5"
  :fg-secondary "#a9b1d6"
  :fg-muted "#565f89"
  :accent "#7aa2f7"
  :accent-hover "#bb9af7"
  :border "#414868"
  :syntax-keyword "#bb9af7"
  :syntax-string "#9ece6a"
  :syntax-comment "#565f89"
  :syntax-number "#ff9e64"
  :syntax-symbol "#c0caf5"
  :syntax-package "#7dcfff"
  :syntax-special "#bb9af7"
  :syntax-error "#f7768e"
  :ansi-black "#15161e"
  :ansi-red "#f7768e"
  :ansi-green "#9ece6a"
  :ansi-yellow "#e0af68"
  :ansi-blue "#7aa2f7"
  :ansi-magenta "#bb9af7"
  :ansi-cyan "#7dcfff"
  :ansi-white "#a9b1d6"
  :ansi-bright-black "#414868"
  :ansi-bright-red "#f7768e"
  :ansi-bright-green "#9ece6a"
  :ansi-bright-yellow "#e0af68"
  :ansi-bright-blue "#7aa2f7"
  :ansi-bright-magenta "#bb9af7"
  :ansi-bright-cyan "#7dcfff"
  :ansi-bright-white "#c0caf5"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: CATPPUCCIN MOCHA
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :catppuccin-mocha
  :display-name "Catppuccin Mocha"
  :dark-p t
  :hl-keyword 183    ; Mauve
  :hl-string 114     ; Green
  :hl-comment 60     ; Overlay0
  :hl-number 215     ; Peach
  :hl-symbol 189     ; Text
  :hl-package 117    ; Sky
  :hl-special 218    ; Pink
  :prompt-primary 111
  :prompt-package 60
  :error 210
  :warning 215
  :info 117
  :dim 60))

(register-browser-theme
 (make-browser-theme
  :name :catppuccin-mocha
  :display-name "Catppuccin Mocha"
  :dark-p t
  :bg-primary "#1e1e2e"
  :bg-secondary "#313244"
  :bg-tertiary "#45475a"
  :fg-primary "#cdd6f4"
  :fg-secondary "#bac2de"
  :fg-muted "#6c7086"
  :accent "#89b4fa"
  :accent-hover "#b4befe"
  :border "#45475a"
  :syntax-keyword "#cba6f7"
  :syntax-string "#a6e3a1"
  :syntax-comment "#6c7086"
  :syntax-number "#fab387"
  :syntax-symbol "#cdd6f4"
  :syntax-package "#89dceb"
  :syntax-special "#f5c2e7"
  :syntax-error "#f38ba8"
  :ansi-black "#45475a"
  :ansi-red "#f38ba8"
  :ansi-green "#a6e3a1"
  :ansi-yellow "#f9e2af"
  :ansi-blue "#89b4fa"
  :ansi-magenta "#cba6f7"
  :ansi-cyan "#89dceb"
  :ansi-white "#bac2de"
  :ansi-bright-black "#585b70"
  :ansi-bright-red "#f38ba8"
  :ansi-bright-green "#a6e3a1"
  :ansi-bright-yellow "#f9e2af"
  :ansi-bright-blue "#89b4fa"
  :ansi-bright-magenta "#cba6f7"
  :ansi-bright-cyan "#89dceb"
  :ansi-bright-white "#a6adc8"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: MONOKAI
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :monokai
  :display-name "Monokai"
  :dark-p t
  :hl-keyword 197    ; Pink
  :hl-string 186     ; Yellow
  :hl-comment 242    ; Comment
  :hl-number 141    ; Purple
  :hl-symbol 231     ; White
  :hl-package 81     ; Blue
  :hl-special 197    ; Pink
  :prompt-primary 148
  :prompt-package 242
  :error 197
  :warning 186
  :info 81
  :dim 242))

(register-browser-theme
 (make-browser-theme
  :name :monokai
  :display-name "Monokai"
  :dark-p t
  :bg-primary "#272822"
  :bg-secondary "#3e3d32"
  :bg-tertiary "#49483e"
  :fg-primary "#f8f8f2"
  :fg-secondary "#f8f8f2"
  :fg-muted "#75715e"
  :accent "#a6e22e"
  :accent-hover "#f92672"
  :border "#49483e"
  :syntax-keyword "#f92672"
  :syntax-string "#e6db74"
  :syntax-comment "#75715e"
  :syntax-number "#ae81ff"
  :syntax-symbol "#f8f8f2"
  :syntax-package "#66d9ef"
  :syntax-special "#f92672"
  :syntax-error "#f92672"
  :ansi-black "#272822"
  :ansi-red "#f92672"
  :ansi-green "#a6e22e"
  :ansi-yellow "#f4bf75"
  :ansi-blue "#66d9ef"
  :ansi-magenta "#ae81ff"
  :ansi-cyan "#a1efe4"
  :ansi-white "#f8f8f2"
  :ansi-bright-black "#75715e"
  :ansi-bright-red "#f92672"
  :ansi-bright-green "#a6e22e"
  :ansi-bright-yellow "#f4bf75"
  :ansi-bright-blue "#66d9ef"
  :ansi-bright-magenta "#ae81ff"
  :ansi-bright-cyan "#a1efe4"
  :ansi-bright-white "#f9f8f5"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: SOLARIZED DARK
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :solarized-dark
  :display-name "Solarized Dark"
  :dark-p t
  :hl-keyword 136    ; Yellow
  :hl-string 37      ; Cyan
  :hl-comment 245    ; Base01
  :hl-number 166     ; Orange
  :hl-symbol 244     ; Base0
  :hl-package 33     ; Blue
  :hl-special 125    ; Magenta
  :prompt-primary 33
  :prompt-package 245
  :error 160
  :warning 136
  :info 37
  :dim 245))

(register-browser-theme
 (make-browser-theme
  :name :solarized-dark
  :display-name "Solarized Dark"
  :dark-p t
  :bg-primary "#002b36"
  :bg-secondary "#073642"
  :bg-tertiary "#586e75"
  :fg-primary "#839496"
  :fg-secondary "#93a1a1"
  :fg-muted "#586e75"
  :accent "#268bd2"
  :accent-hover "#2aa198"
  :border "#073642"
  :syntax-keyword "#b58900"
  :syntax-string "#2aa198"
  :syntax-comment "#586e75"
  :syntax-number "#cb4b16"
  :syntax-symbol "#839496"
  :syntax-package "#268bd2"
  :syntax-special "#d33682"
  :syntax-error "#dc322f"
  :ansi-black "#073642"
  :ansi-red "#dc322f"
  :ansi-green "#859900"
  :ansi-yellow "#b58900"
  :ansi-blue "#268bd2"
  :ansi-magenta "#d33682"
  :ansi-cyan "#2aa198"
  :ansi-white "#eee8d5"
  :ansi-bright-black "#002b36"
  :ansi-bright-red "#cb4b16"
  :ansi-bright-green "#586e75"
  :ansi-bright-yellow "#657b83"
  :ansi-bright-blue "#839496"
  :ansi-bright-magenta "#6c71c4"
  :ansi-bright-cyan "#93a1a1"
  :ansi-bright-white "#fdf6e3"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: GITHUB LIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :github-light
  :display-name "GitHub Light"
  :dark-p nil
  :hl-keyword 127    ; Purple
  :hl-string 22      ; Green
  :hl-comment 244    ; Gray
  :hl-number 166     ; Orange
  :hl-symbol 16      ; Black
  :hl-package 25     ; Blue
  :hl-special 127    ; Purple
  :prompt-primary 25
  :prompt-package 244
  :error 160
  :warning 166
  :info 25
  :dim 244))

(register-browser-theme
 (make-browser-theme
  :name :github-light
  :display-name "GitHub Light"
  :dark-p nil
  :bg-primary "#ffffff"
  :bg-secondary "#f6f8fa"
  :bg-tertiary "#e1e4e8"
  :fg-primary "#24292e"
  :fg-secondary "#586069"
  :fg-muted "#6a737d"
  :accent "#0366d6"
  :accent-hover "#0056b3"
  :border "#e1e4e8"
  :syntax-keyword "#d73a49"
  :syntax-string "#032f62"
  :syntax-comment "#6a737d"
  :syntax-number "#005cc5"
  :syntax-symbol "#24292e"
  :syntax-package "#6f42c1"
  :syntax-special "#d73a49"
  :syntax-error "#cb2431"
  :ansi-black "#24292e"
  :ansi-red "#d73a49"
  :ansi-green "#22863a"
  :ansi-yellow "#b08800"
  :ansi-blue "#0366d6"
  :ansi-magenta "#6f42c1"
  :ansi-cyan "#1b7c83"
  :ansi-white "#fafbfc"
  :ansi-bright-black "#586069"
  :ansi-bright-red "#cb2431"
  :ansi-bright-green "#28a745"
  :ansi-bright-yellow "#dbab09"
  :ansi-bright-blue "#2188ff"
  :ansi-bright-magenta "#8a63d2"
  :ansi-bright-cyan "#3192aa"
  :ansi-bright-white "#ffffff"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: SOLARIZED LIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :solarized-light
  :display-name "Solarized Light"
  :dark-p nil
  :hl-keyword 136    ; Yellow
  :hl-string 37      ; Cyan
  :hl-comment 245    ; Base1
  :hl-number 166     ; Orange
  :hl-symbol 240     ; Base00
  :hl-package 33     ; Blue
  :hl-special 125    ; Magenta
  :prompt-primary 33
  :prompt-package 245
  :error 160
  :warning 136
  :info 37
  :dim 245))

(register-browser-theme
 (make-browser-theme
  :name :solarized-light
  :display-name "Solarized Light"
  :dark-p nil
  :bg-primary "#fdf6e3"
  :bg-secondary "#eee8d5"
  :bg-tertiary "#93a1a1"
  :fg-primary "#657b83"
  :fg-secondary "#586e75"
  :fg-muted "#93a1a1"
  :accent "#268bd2"
  :accent-hover "#2aa198"
  :border "#eee8d5"
  :syntax-keyword "#b58900"
  :syntax-string "#2aa198"
  :syntax-comment "#93a1a1"
  :syntax-number "#cb4b16"
  :syntax-symbol "#657b83"
  :syntax-package "#268bd2"
  :syntax-special "#d33682"
  :syntax-error "#dc322f"
  :ansi-black "#073642"
  :ansi-red "#dc322f"
  :ansi-green "#859900"
  :ansi-yellow "#b58900"
  :ansi-blue "#268bd2"
  :ansi-magenta "#d33682"
  :ansi-cyan "#2aa198"
  :ansi-white "#eee8d5"
  :ansi-bright-black "#002b36"
  :ansi-bright-red "#cb4b16"
  :ansi-bright-green "#586e75"
  :ansi-bright-yellow "#657b83"
  :ansi-bright-blue "#839496"
  :ansi-bright-magenta "#6c71c4"
  :ansi-bright-cyan "#93a1a1"
  :ansi-bright-white "#fdf6e3"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: GRUVBOX LIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :gruvbox-light
  :display-name "Gruvbox Light"
  :dark-p nil
  :hl-keyword 132    ; Purple
  :hl-string 100     ; Green
  :hl-comment 244    ; Gray
  :hl-number 166     ; Orange
  :hl-symbol 237     ; Fg
  :hl-package 66     ; Blue
  :hl-special 132    ; Purple
  :prompt-primary 136
  :prompt-package 244
  :error 124
  :warning 136
  :info 66
  :dim 244))

(register-browser-theme
 (make-browser-theme
  :name :gruvbox-light
  :display-name "Gruvbox Light"
  :dark-p nil
  :bg-primary "#fbf1c7"
  :bg-secondary "#ebdbb2"
  :bg-tertiary "#d5c4a1"
  :fg-primary "#3c3836"
  :fg-secondary "#504945"
  :fg-muted "#928374"
  :accent "#d65d0e"
  :accent-hover "#af3a03"
  :border "#d5c4a1"
  :syntax-keyword "#8f3f71"
  :syntax-string "#79740e"
  :syntax-comment "#928374"
  :syntax-number "#d65d0e"
  :syntax-symbol "#3c3836"
  :syntax-package "#076678"
  :syntax-special "#8f3f71"
  :syntax-error "#9d0006"
  :ansi-black "#fbf1c7"
  :ansi-red "#cc241d"
  :ansi-green "#98971a"
  :ansi-yellow "#d79921"
  :ansi-blue "#458588"
  :ansi-magenta "#b16286"
  :ansi-cyan "#689d6a"
  :ansi-white "#7c6f64"
  :ansi-bright-black "#928374"
  :ansi-bright-red "#9d0006"
  :ansi-bright-green "#79740e"
  :ansi-bright-yellow "#b57614"
  :ansi-bright-blue "#076678"
  :ansi-bright-magenta "#8f3f71"
  :ansi-bright-cyan "#427b58"
  :ansi-bright-white "#3c3836"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Built-in Themes: ONE LIGHT
;;; ─────────────────────────────────────────────────────────────────────────────

(register-terminal-theme
 (make-terminal-theme
  :name :one-light
  :display-name "One Light"
  :dark-p nil
  :hl-keyword 128    ; Purple
  :hl-string 22      ; Green
  :hl-comment 244    ; Gray
  :hl-number 166     ; Orange
  :hl-symbol 236     ; Fg
  :hl-package 31     ; Cyan
  :hl-special 128    ; Purple
  :prompt-primary 31
  :prompt-package 244
  :error 160
  :warning 166
  :info 31
  :dim 244))

(register-browser-theme
 (make-browser-theme
  :name :one-light
  :display-name "One Light"
  :dark-p nil
  :bg-primary "#fafafa"
  :bg-secondary "#eaeaeb"
  :bg-tertiary "#dbdbdc"
  :fg-primary "#383a42"
  :fg-secondary "#4b4e55"
  :fg-muted "#a0a1a7"
  :accent "#4078f2"
  :accent-hover "#526fff"
  :border "#dbdbdc"
  :syntax-keyword "#a626a4"
  :syntax-string "#50a14f"
  :syntax-comment "#a0a1a7"
  :syntax-number "#986801"
  :syntax-symbol "#383a42"
  :syntax-package "#0184bc"
  :syntax-special "#a626a4"
  :syntax-error "#e45649"
  :ansi-black "#383a42"
  :ansi-red "#e45649"
  :ansi-green "#50a14f"
  :ansi-yellow "#c18401"
  :ansi-blue "#4078f2"
  :ansi-magenta "#a626a4"
  :ansi-cyan "#0184bc"
  :ansi-white "#fafafa"
  :ansi-bright-black "#a0a1a7"
  :ansi-bright-red "#e45649"
  :ansi-bright-green "#50a14f"
  :ansi-bright-yellow "#c18401"
  :ansi-bright-blue "#4078f2"
  :ansi-bright-magenta "#a626a4"
  :ansi-bright-cyan "#0184bc"
  :ansi-bright-white "#ffffff"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; User-facing Theme Functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun set-terminal-theme (name)
  "Set the terminal theme by name (keyword or string)."
  (when (stringp name)
    (setf name (intern (string-upcase name) :keyword)))
  (apply-terminal-theme name))

(defun set-browser-theme (name)
  "Set the browser theme by name (keyword or string)."
  (when (stringp name)
    (setf name (intern (string-upcase name) :keyword)))
  (apply-browser-theme name))

(defun current-terminal-theme-name ()
  "Get the name of the current terminal theme."
  (when *current-terminal-theme*
    (terminal-theme-name *current-terminal-theme*)))

(defun current-browser-theme-name ()
  "Get the name of the current browser theme."
  (when *current-browser-theme*
    (browser-theme-name *current-browser-theme*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Initialization
;;; ─────────────────────────────────────────────────────────────────────────────

(defun initialize-themes ()
  "Initialize the theming system with auto-detection."
  ;; Auto-select terminal theme based on dark mode
  (auto-select-terminal-theme)
  ;; Auto-select browser theme based on same detection
  ;; (will be overridden when client reports its own preference)
  (auto-select-browser-theme))

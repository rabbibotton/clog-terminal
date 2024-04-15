(defpackage #:clog-terminal
  (:use #:cl #:clog)
  (:export clog-terminal-element
           create-clog-terminal-element
           set-on-command
           prompt
           echo exec clear reset
           init-clog-terminal
           attach-clog-terminal
           start-test))

(in-package :clog-terminal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-terminal-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-terminal-element (clog-element)()
  (:documentation "CLOG Terminal Element Object."))

(defgeneric create-clog-terminal-element (clog-obj &key hidden class html-id auto-place)
  (:documentation "Create a new clog-terminal-element as child of CLOG-OBJ."))

(defmethod create-clog-terminal-element ((obj clog:clog-obj)
                                         &key
                                           (greetings "") (prompt "> ")
                                           (hidden nil)
                                           (class nil)
                                           (html-id nil)
                                           (auto-place t))
  (let ((new-obj (create-div obj
                             :class class
                             :hidden hidden
                             :html-id html-id
                             :auto-place auto-place)))
    (set-geometry new-obj :width 200 :height 100)
    (attach-clog-terminal new-obj :greetings greetings :prompt prompt)
    (change-class new-obj 'clog-terminal-element)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events - clog-terminal-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-command (clog-terminal-element handler
                            &key cancel-event one-time)
  (:documentation "Set a HANDLER for clog-terminal commands on CLOG-OBJ.
 If HANDLER is nil unbind all event handlers. Handler is called with a
data parameter with what was entered on command line."))

(defmethod set-on-command ((obj clog-terminal-element) handler
                             &key
                               (cancel-event nil)
                               (one-time     nil))
  (set-on-event-with-data obj "clog-terminal-command"
                          (lambda (obj data)
                            (funcall handler obj data))
                          :cancel-event cancel-event
                          :one-time      one-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-terminal-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;
;; prompt ;;
;;;;;;;;;;;;

(defgeneric prompt (clog-terminal-element text)
  (:documentation "Set prompt text for terminal (write only)"))

(defmethod prompt ((obj clog-terminal-element) text)
  (jquery-execute obj (format nil "terminal().set_prompt('~A')"
                              (escape-string text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-terminal-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; echo ;;
;;;;;;;;;;

(defgeneric echo (clog-terminal-element text)
  (:documentation "Echo text to terminal"))

(defmethod echo ((obj clog-terminal-element) text)
  (jquery-execute obj (format nil "terminal().echo('~A')"
                              (escape-string text))))

;;;;;;;;;;
;; exec ;;
;;;;;;;;;;

(defgeneric exec (clog-terminal-element text)
  (:documentation "Exec text as if command entered at terminal"))

(defmethod exec ((obj clog-terminal-element) text)
  (jquery-execute obj (format nil "terminal().exec('~A')"
                              (escape-string text))))

;;;;;;;;;;;
;; clear ;;
;;;;;;;;;;;

(defgeneric clear (clog-terminal-element)
  (:documentation "Clear text on terminal"))

(defmethod clear ((obj clog-terminal-element))
  (jquery-execute obj "terminal().clear()"))

;;;;;;;;;;;
;; reset ;;
;;;;;;;;;;;

(defgeneric reset (clog-terminal-element)
  (:documentation "Reset text on terminal"))

(defmethod reset ((obj clog-terminal-element))
  (jquery-execute obj "terminal().reset()"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - js binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-clog-terminal (obj)
  (check-type obj clog:clog-obj)
  (let ((local (format nil "~A/clog-terminal" clog:*static-root*)))
    (cond ((uiop:directory-exists-p local)
           (load-css (html-document (connection-data-item obj "clog-body"))
                     "/clog-terminal/jquery.terminal.min.css" :load-only-once t)
           (load-script (html-document (connection-data-item obj "clog-body"))
                        "/clog-terminal/jquery.terminal.min.js" :load-only-once t)
           (load-script (html-document (connection-data-item obj "clog-body"))
                      "/clog-terminal/unix_formatting.js" :load-only-once t))
          (t
           (load-css (html-document (connection-data-item obj "clog-body"))
                     "https://cdn.jsdelivr.net/npm/jquery.terminal/css/jquery.terminal.min.css" :load-only-once t)
           (load-script (html-document (connection-data-item obj "clog-body"))
                        "https://cdn.jsdelivr.net/npm/jquery.terminal/js/jquery.terminal.min.js" :load-only-once t)
           (load-script (html-document (connection-data-item obj "clog-body"))
                      "https://cdn.jsdelivr.net/npm/jquery.terminal/js/unix_formatting.js" :load-only-once t)))))

(defun attach-clog-terminal (obj &key (greetings "") (prompt "> "))
  (init-clog-terminal obj)
  (js-execute obj
              (format nil "~
    ~A.terminal(function(command) {
      ~A.trigger('clog-terminal-command', command);
    }, {
        prompt: '~A',
        greetings: '~A'
    });"
                      (jquery obj)
                      (jquery obj)
                      (escape-string prompt)
                      (escape-string greetings))))

(defun on-test-clog-terminal (body)
  (clog:debug-mode body)
  (let* ((layout (create-panel-box-layout body))
         (term   (create-clog-terminal-element (center-panel layout)
                                               :greetings "Test Terminal")))
    (center-children (center-panel layout))
    (set-geometry term :units "%" :height 100 :width 100)
    (set-border term :thin :solid :black)
    (set-on-command term (lambda (obj data)
                           (echo obj data)))))

(defun start-test ()
  (initialize 'on-test-clog-terminal
   :static-root (merge-pathnames "./www/"
                  (asdf:system-source-directory :clog-terminal)))
  (open-browser))

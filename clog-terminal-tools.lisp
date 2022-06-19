(in-package :clog-terminal)

(progn
  (clog-tools:add-supported-controls
   (list `(:name           "clog-terminal"
	   :description    "clog-terminal jQueryTerminal"
	   :clog-type      clog-terminal:clog-terminal-element
	   ;; the create-function used to create the function
	   ;; at _design time_ at run time only clog:attach-as-child is used
	   ;; any initialization at _run time_ is done with :on-setup below.
	   :create         clog-terminal:create-clog-terminal-design
	   ;; clog has the following create-types
	   ;;   :base         - create
	   ;;   :element      - create create-content
	   ;;   :form         - create create-param create-value
	   ;;   :text-area    - create create-value
	   ;;   :custom-query - create (ask user for string)
	   ;;   :custom       - create create-content
	   :create-type    :base
	   ;; setup the control at _design time_ and custom attributes
	   :setup          ,(lambda (control content control-record)
                              (declare (ignore content) (ignore control-record))
			      ;; default custom attribute values at design time
                              (setf (attribute control "data-clog-terminal-prompt") "> ")
                              (setf (attribute control "data-clog-terminal-greetings") "Terminal Ready")
                              (setf (attribute control "data-on-command")
				    "(clog-terminal:echo target data)"))
	   ;; code to run at _run time_ after all controls attached to panel
	   :on-setup       ,(lambda (control control-record)
                              (declare (ignore control control-record))
			      ;; initialization at run time and apply custom attributes
			      (format nil "(clog-terminal:attach-clog-terminal target :greetings \"~A\") ~
                                           (clog-terminal:prompt target \"~A\")"
				      (attribute control "data-clog-terminal-greetings")
				      (attribute control "data-clog-terminal-prompt")))
	   ;; events handled
           :events         ((:name        "on-command"
                             :package     "clog-terminal"
                             :parameters  "target data")
			    ,@clog-tools::*events-element*)
	   ;; properties handled
	   :properties     ((:name "terminal greetings"
			     :attr "data-clog-terminal-greetings")
			    (:name "terminal prompt"
			     :attr "data-clog-terminal-prompt")
			    ,@clog-tools::*props-element*))))
  (format t "~%CLOG-TERMINAL installed in CLOG Builder"))

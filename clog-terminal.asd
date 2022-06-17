(asdf:defsystem #:clog-terminal
  :description "CLOG Terminal"
  :author "david@botton.com"
  :license  "BSD"
  :version "0.1.0"
  :serial t
  :depends-on (#:clog)
  :components ((:file "clog-terminal")))

(asdf:defsystem #:clog-terminal/tools
  :depends-on (#:clog-terminal #:clog/tools)
  :components ((:file "clog-terminal-tools")))

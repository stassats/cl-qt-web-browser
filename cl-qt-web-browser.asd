(defsystem cl-qt-web-browser
  :depends-on (qt cl-ppcre)
  :serial t 
  :components
  ((:file "packages")
   (:file "browser")))

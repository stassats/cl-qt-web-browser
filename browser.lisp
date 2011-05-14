(in-package qt-web)
(named-readtables:in-readtable :qt)

(defun add-widgets (layout &rest widgets)
  (dolist (widget widgets)
    (#_addWidget layout widget)))

(defmacro with-layout ((var name &optional parent-layout) &body body)
  `(let ((,var (optimized-new ,name)))
     ,(when parent-layout
            `(optimized-call t ,parent-layout "addLayout"
                             ,var))
     ,@body))

;;;

(defclass browser ()
  ((tab-bar :accessor tab-bar)
   (tabs :accessor tabs :initform nil)
   (stack :initform nil))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("browseUrl()" browse-url)
          ("updateUrl()" update-url)
          ("selectTab(int)" select-tab)
          ("newTab()" new-tab)
          ("updateTitles()" update-titles)
          ("closeTab(int)" close-tab)))

(defclass q-tab-bar ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QTabBar")
  (:override ("mouseReleaseEvent" mouse-release-event)))

(defmethod initialize-instance :after ((instance q-tab-bar) &key parent)
  (if parent
      (new instance parent)
      (new instance)))

(defmethod mouse-release-event ((tab-bar q-tab-bar) event)
  (when (enum= (#_Qt::MiddleButton)
               (#_button event))
    (emit-signal tab-bar
                 "tabCloseRequested(int)"
                 (#_tabAt tab-bar (#_pos event)))))

(defmethod initialize-instance :after ((instance browser) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((layout (#_new QVBoxLayout))
        (tab-bar (make-instance 'q-tab-bar))
        (new-tab (#_new QPushButton "+"))
        (stack (#_new QStackedLayout)))

    (setf (tab-bar instance) tab-bar
          (slot-value instance 'stack) stack)

    (connect new-tab "clicked()"
             instance "newTab()")
    (connect tab-bar "currentChanged(int)"
             instance "selectTab(int)")
    (connect tab-bar "currentChanged(int)"
             instance "selectTab(int)")
    (connect tab-bar "tabCloseRequested(int)"
             instance "closeTab(int)")

    (#_setLayout instance layout)

    (with-layout (hbox "QHBoxLayout" layout)
      (add-widgets hbox new-tab tab-bar)
      (#_addStretch hbox))

    (#_addLayout layout stack)

    (#_setExpanding tab-bar nil)
    (new-tab instance)))

(defclass q-web-view ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QWebView")
  (:override ("createWindow" create-tab)))

(defclass browser-tab ()
  ((web-view :accessor web-view
             :initform nil)
   (address-bar :accessor address-bar)
   (back-history :initform nil)
   (forward-history :initform nil)
   (tab-index :initarg :tab-index
              :initform nil
              :accessor tab-index)
   back-button
   forward-button)
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("browseUrl()" browse-url)
          ("updateUrl(QUrl)" update-url)
          ("updateTitles()" update-titles)
          ("updateIcon()" update-icon)
          ("createTab(QWebPage::WebWindowType)" create-tab)))

(defmethod initialize-instance :after ((instance q-web-view) &key parent)
  (if parent
      (new instance parent)
      (new instance)))

(defmethod initialize-instance :after ((instance browser-tab) &key parent)
  (if parent
      (new instance parent)
      (new instance))
  (let ((layout (#_new QVBoxLayout))
        (web-view (make-instance 'q-web-view :parent instance))
        (toolbar (#_new QToolBar))
        (address (#_new QLineEdit))
        (go (#_new QToolButton))
        (reload (#_new QToolButton))
        (back (#_new QToolButton))
        (forward (#_new QToolButton))
        (progress (#_new QProgressBar)))
    (set-settings web-view)
    (setf (web-view instance) web-view
          (address-bar instance) address)

    (#_setRange progress 0 100)
    (connect address "returnPressed()"
             instance "browseUrl()")
    (connect go "clicked()"
             address (qsignal "returnPressed()"))
    (connect reload "clicked()"
             web-view "reload()")
    (connect back "clicked()"
             web-view "back()")

    (connect web-view "loadProgress(int)"
             progress "setValue(int)")
    (connect web-view "loadStarted()"
             progress "show()")

    (connect web-view "loadFinished(bool)"
             progress "hide()")

    (connect web-view "urlChanged(QUrl)"
             instance "updateUrl(QUrl)")
    (connect web-view "titleChanged(QString)"
             parent "updateTitles()")

    (connect web-view "iconChanged()"
             instance "updateIcon()")

    (#_setLayout instance layout)
    (add-widgets layout toolbar web-view progress)
    (add-widgets toolbar back forward reload address go)

    (#_setIcon back (#_standardIcon (#_style instance) (#_QStyle::SP_ArrowBack)))
    (#_setIcon forward (#_standardIcon (#_style instance) (#_QStyle::SP_ArrowForward)))
    (#_setIcon reload (#_standardIcon (#_style instance) (#_QStyle::SP_BrowserReload)))

    (#_setPopupMode back (#_QToolButton::MenuButtonPopup))
    (#_setPopupMode forward (#_QToolButton::MenuButtonPopup))
    (#_setMenu back (#_new QMenu))
    (#_setMenu forward (#_new QMenu))

    (#_hide progress)
    (#_setFocus address)))

(defun set-settings (webpage)
  (let ((settings (#_settings webpage)))
    (#_setAttribute settings
                    (#_QWebSettings::PluginsEnabled) t)
    (#_setIconDatabasePath settings
                           (ensure-directories-exist "/tmp/cl-qt-browser/"))))

(defun append-http (url)
  (let ((url (string-trim '(#\Space) url)))
    (if (ppcre:scan "^.+?://" url)
        url
        (concatenate 'string "http://" url))))

(defun browse-url (instance)
  (let ((url (append-http (#_text (address-bar instance)))))
    (#_load (web-view instance)
            (#_new QUrl url))))

(defun close-tab (browser index)
  (#_removeTab (tab-bar browser) index)
  (setf (tabs browser)
        (loop for tab in (tabs browser)
              when (/= (tab-index tab) index)
              collect tab
              when (> (tab-index tab) index)
              do (decf (tab-index tab)))))

(defmethod create-tab ((web-view q-web-view) window-type)
  (let ((tab (new-tab (#_parent (#_parent web-view)))))
    (web-view tab)))

(defun new-tab (window)
  (with-slots (tabs tab-bar stack) window
    (let* ((new-index (length tabs))
           (tab (make-instance 'browser-tab :parent window
                               :tab-index new-index)))
      (#_addWidget stack tab)
      (push tab tabs)
      (#_addTab tab-bar "blank")
      (#_setCurrentIndex tab-bar new-index)
      tab)))

(defun nth-tab (n tabs)
  (nth (- (length tabs) n 1) tabs))

(defun tab-position (tab tabs)
  (let ((position (position tab tabs :test #'eq)))
    (when position
      (- (length tabs) position 1))))

(defun select-tab (instance n)
  (when (>= n 0)
    (with-slots (tabs stack) instance
      (let ((tab (nth-tab n tabs)))
        (#_setCurrentWidget stack tab)
        (#_setWindowTitle instance (#_title (web-view tab)))))))

(defun update-titles (instance)
  (with-slots (tab-bar tabs) instance
    (loop with current = (#_currentIndex tab-bar)
          for tab in tabs
          for index from (1- (length tabs)) downto 0
          for title = (#_title (web-view tab))
          when (= index current) do
          (#_setWindowTitle instance title)
          do (#_setTabText tab-bar index title))))

(defun update-icon (instance)
  (let ((tab-bar (tab-bar (#_parent instance))))
    (#_setTabIcon tab-bar (tab-index instance)
                  (#_icon (web-view instance)))))

(defun update-url (tab qurl)
  (#_setText (address-bar tab) (#_toString qurl)))

(defun start-from-image ()
  (qt::reload)
  (ensure-smoke :qtwebkit)
  (web))

(defvar *qapp*)

(defun web ()
  (ensure-smoke :qtwebkit)
  (setf *qapp* (make-qapplication))
  (let ((window (make-instance 'browser)))
    (#_show window)
    (unwind-protect
         (#+sbcl sb-int:with-float-traps-masked #+sbcl (:invalid :divide-by-zero)
                 #-sbcl progn
                 (#_exec *qapp*))
      (#_hide window))))

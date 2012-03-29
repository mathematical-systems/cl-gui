(in-package :cl-gui)

(defmacro+ps ext-require-all ()
  `(chain *Ext (require '("*"))))

(defmacro+ps ext-require (modules)
  `(chain *Ext (require ,modules)))

(defmacro+ps ext-enable-loader ()
  `(chain *Ext *Loader (set-config (create :enabled t))))

(defmacro+ps ext-loader-setpath (namespace path)
  `(chain *Ext *Loader (set-path ,namespace ,path)))

;;;
(defmacro+ps def-ext-main (args &body body)
  (declare (ignorable args))
  `(chain *Ext (on-ready (lambda () ,@body))))

(defmacro+ps make-ext-instance (class &optional initargs)
  `(chain *Ext (create ,class ,@(when initargs (list initargs)))))

(defmacro+ps make-ext-menu (&key id items)
  `(make-ext-instance "Ext.menu.Menu"
                      (create id ,id
                              items ,items)))

(defmacro+ps make-ext-toolbar (&key id items)
  `(make-ext-instance "Ext.toolbar.Toolbar"
                      (create id ,id
                              items ,items)))

(defmacro+ps ext-define (name properties)
  `(chain *Ext (define ,name ,properties)))

;;;
(defmacro+ps ext-getbody ()
  `(chain *Ext (get-body)))

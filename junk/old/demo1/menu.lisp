(ext-require-all)

(def-ext-main ()
  (let* ((date-menu (make-ext-instance "Ext.menu.DatePicker"))
         (color-menu (make-ext-instance "Ext.menu.ColorPicker"))
         (file-menu (make-ext-menu :id "fileMenu"
                                   :items (list (create :text "Load data"
                                                        :handler (lambda ()
                                                                   (funcall
                                                                    (@ lisp emit)
                                                                    "event"
                                                                    "load-data")))
                                                (create :text "Show visualization"
                                                        :handler #'show-vis))))
         (pref-menu (make-ext-menu :id "prefMenu"
                                   :items (list (create :text "Theme"
                                                        :menu (create :items (list (create :text "Aero Glass"
                                                                                           :checked t
                                                                                           :group "theme")
                                                                                   (create :text "Vista Black"
                                                                                           :checked nil
                                                                                           :group "theme")
                                                                                   (create :text "Gray Theme"
                                                                                           :checked nil
                                                                                           :group "theme")
                                                                                   (create :text "Default Theme"
                                                                                           :checked nil
                                                                                           :group "theme"))))
                                                (create :text "Date"
                                                        :menu date-menu)
                                                (create :text "Color"
                                                        :menu color-menu))))
         (help-menu (make-ext-menu :id "helpMenu"
                                   :items (list (create :text "About"))))
         (menu-bar (make-ext-toolbar :id "menuBar"
                                     :items (list (create :text "File"
                                                          :menu file-menu)
                                                  (create :text "Options"
                                                          :menu pref-menu)
                                                  (create :text "Help"
                                                          :menu help-menu)))))
    ;;
    (chain menu-bar (render "menubar"))))


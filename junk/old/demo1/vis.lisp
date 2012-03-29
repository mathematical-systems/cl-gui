(defvar label-type "Native")
(defvar use-gradients t)
(defvar native-text-support t)
(defvar animate t)
(defvar data)
(defvar fd)

(defun create-vis ()
  (funcall
   (new (@ $jit *force-directed))
   (create
    inject-into "infovis"
    width 1200
    height 800
    |Navigation| (create enable t
                         panning "avoid nodes"
                         zooming 10)
    |Node| (create overridable t)
    |Edge| (create overridable t
                   color "#23A4FF"
                   line-width 0.4)
    |Label| (create type label-type
                    size 10
                    style "bold"
                    color "#000")
    |Tips| (create enable t
                   on-show (lambda (tip node)
                             (let ((count 0))
                               (chain node (each-adjacency (lambda () (incf count))))
                               (setf (@ tip |innerHTML|)
                                     (+ "<div class=\"tip-title\">"
                                        (@ node name)
                                        "</div>"
                                        "<div class=\"tip-text\"><b>connections:</b> "
                                        count
                                        "</div>")))))
    |Events| (create enable t
                     type "Native"
                     on-mouse-enter
                     (lambda ()
                       (setf (chain fd canvas (get-element) style cursor) "move"))
                     on-mouse-leave
                     (lambda ()
                       (setf (chain fd canvas (get-element) style cursor) ""))
                     on-drag-move
                     (lambda (node event-info e)
                       (let ((pos (chain event-info (get-pos))))
                         (chain node pos (setc (@ pos x) (@ pos y)))
                         (chain fd (plot))))
                     on-touch-move
                     (lambda (node event-info e)
                       (chain $jit util event (stop e))
                       (chain this (on-drag-move node event-info e)))
                     on-click
                     (lambda (node)
                       (if (not node) (return))
                       (let ((html (+ "<h4>" (@ node name) "</h4><b> connections:</b><ul><li>"))
                             (list '()))
                         (chain node (each-adjacency
                                      (lambda (adj)
                                        (chain list (push (@ adj node-to name)))))))
                       (setf (chain $jit (id "inner-details") |innerHTML|)
                             (+ html (chain list (join "</li><li>")) "</li></ul>"))))
    iterations 200
    level-distance 130
    on-create-label (lambda (dom-element node)
                      (setf (@ dom-element |innerHTML|) (@ node name))
                      (let ((style (@ dom-element style)))
                        (setf (@ style font-size) "0.8em")
                        (setf (@ style color) "#ddd")))
    on-place-label (lambda (dom-element node)
                     (let* ((style (@ dom-element style))
                            (left (parse-int (@ style left)))
                            (top (parse-int (@ style top)))
                            (w (@ dom-element offset-width)))
                       (setf (@ style left) (+ (- left (/ w 2)) "px")
                             (@ style top) (+ top 10 "px")
                             (@ style display) ""))))))

(defun show-vis ()
  (setf fd (create-vis))
  ;;
  ;; $.getJSON("data.js", function(result) {data=result});
  (dolist (d lisp-data)
    (when (and d (@ d graph))
      (setf data (@ d graph))
      (break)))
  ;;
  (chain fd (|loadJSON| data))
  ;;
  (funcall
   (@ fd compute-incremental)
   (create iter 40
           property "end"
           on-step (lambda (perc))
           on-complete (lambda ()
                         (funcall (@ fd animate)
                                  (create modes '("linear")
                                          transition (@ $jit |Trans| |Elastic| ease-out)
                                          duration 2500))))))


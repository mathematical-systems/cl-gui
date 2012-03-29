(in-package :cl-gui)

(defclass twitter ()
  ((screen-name :initarg :screen-name :accessor screen-name-of)
   (statuses :initarg :statuses :accessor statuses-of)
   (friends-network :initarg :friends-network :initform nil :accessor friends-network-of))
  (:metaclass js-class))

(defun make-link (from to)
  `((:node-from . ,from)
    (:node-to . ,(write-to-string to))))

(defun make-node (name tos)
  (let* ((adjacencies (loop for to in tos
                            collect (make-link name to))))
    `((:name . ,name)
      (:id . ,name)
      (:data (:$type . "circle")
             (:$dim . "5"))
      (:adjacencies . ,adjacencies))))

(defun load-twitter-data (username)
  "Return ((date status) ...)"
  (let* ((statuses (nth-value 2 (twitter.crawler::get-user-twits username :count 100))))
    (make-instance 'twitter :_id "twitter"
                            :screen-name username
                            :statuses statuses)))

(defun load-friend-ids (username breadth)
  (let* ((obj (get-js-object "twitter"))
         (friends (subseq (twitter.crawler::get-user-friend-ids username) 0 breadth))
         (fof (mapcar (lambda (id)
                        (subseq (twitter.crawler::get-user-friend-ids id) 0 breadth))
                      friends))
         (network (loop for n in (cons username friends)
                        for tos in (cons friends fof)
                        collect (make-node n tos))))
    (setf (friends-network-of obj) network)
    (update-js-object obj)))

(defun update-statuses (id)
  (when id
    (let* ((statuses (nth-value 2 (twitter.crawler::get-user-twits id :count 100))))
      (make-instance 'twitter :_id "statuses update"
                              :screen-name id
                              :statuses statuses))))


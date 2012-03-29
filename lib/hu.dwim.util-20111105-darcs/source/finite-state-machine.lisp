;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.util)

;;;;;;
;;; Model

(def class* finite-state-machine ()
  ((name
    :type symbol
    :documentation "The name used to identify this state machine.")
   (states
    :type list
    :documentation "The list of possible states.")
   (events
    :type list
    :documentation "The union of all possible events from all states.")
   (transitions
    :type list
    :documentation "The union of all possible transitions from all states.")
   (start-state
    :type start-state
    :documentation "The initial state that is used when the state machine instance is created.")
   (stop-states
    :type list
    :documentation "The list of all possible final states.")
   (state-type
    :type t
    :documentation "The lisp type of possible states."))
  (:documentation "A finite state machine is a process described via states and transitions between these states fired upon incoming events."))

(def class* finite-state-machine-element ()
  ((name
    :type symbol
    :documentation "The name used to identify this element within its state machine.")
   (state-machine
    :type finite-state-machine
    :documentation "The state machine that this element belongs to."))
  (:documentation "Finite state machine elements exclusively belong to a finite state machine."))

(def class* state (finite-state-machine-element)
  ((on-enter-action
    :type function
    :documentation "The on enter action is called whenever this state becomes the new state of the finite state machine.")
   (on-leave-action
    :type function
    :documentation "The on leave action is called whenever the finite state machine leaves this state.")
   (on-stay-action
    :type function
    :documentation "The on stay action is called whenever the finite state machine stays in this state during a transition.")
   (transitions
    :type list
    :documentation "The list of possible transitions within this state.")))

(def class* start-state (state)
  ()
  (:documentation "There is only one start state in a finite state machine. This is the state of a new state machine instance when it is created."))

(def class* stop-state (state)
  ()
  (:documentation "There might be several stop states in a finite state machine."))

(def class* transition (finite-state-machine-element)
  ((event
    :type event
    :documentation "The event that triggers this transition if the finite state machine is in the source state.")
   (source
    :type state
    :documentation "The source state of the transition.")
   (target
    :type state
    :documentation "The target state of the transition.")
   (action
    :type function
    :documentation "The action is called when this transition fires."))
  (:documentation "A transition specifies what should happen if a specific event occurs when the finite state machine is in the source state."))

(def class* event (finite-state-machine-element)
  ()
  (:documentation "An event identifies a trigger that fires a certain transition depending on the state machine's current state."))

(def print-object finite-state-machine-element
  (if (slot-boundp -self- 'name)
      (princ (name-of -self-))
      (princ "<<name unbound>>")))

;;;;;;
;;; Defining

(def (namespace e) finite-state-machine)

(def macro def-finite-state-machine (name &body forms)
  (bind ((states (cdr (find :states forms :key #'first)))
         (state-names (mapcar (lambda (state) (first (ensure-list state))) states))
         (start-states (collect-if (lambda (state) (getf (cdr state) :start-state)) states))
         (stop-states (collect-if (lambda (state) (getf (cdr state) :stop-state)) states))
         (transitions (cdr (find :transitions forms :key #'first)))
         (events (delete-duplicates (mapcar #'first transitions))))
    `(bind (,@(mapcar (lambda (state name)
                        (bind ((class-name (cond ((member state start-states)
                                                  'start-state)
                                                 ((member state stop-states)
                                                  'stop-state)
                                                 (t
                                                  'state))))
                          `(,name (make-instance ',class-name :name ',name))))
                      states
                      state-names)
              ,@(mapcar (lambda (event)
                          `(,event (make-instance 'event :name ',event)))
                        events))
       (setf (find-finite-state-machine ',name)
             (make-instance 'finite-state-machine
                            :name ',name
                            :states (list ,@state-names)
                            :state-type `(member ,,@state-names)
                            :events (list ,@events)
                            :transitions (list ,@(mapcar (lambda (transition)
                                                           `(make-instance 'transition
                                                                           :name ',(format-symbol (symbol-package (second transition))
                                                                                                  "~A->~A" (second transition) (third transition))
                                                                           :event ,(first transition)
                                                                           :source ,(second transition)
                                                                           :target ,(third transition)))
                                                         transitions)))))))

(def (definer e :available-flags "e") finite-state-machine (name &body forms)
  (with-standard-definer-options name
    `(def-finite-state-machine ,name ,@forms)))

(def method shared-initialize :after ((fsm finite-state-machine) slot-names &key &allow-other-keys)
  (dolist (element (append (states-of fsm) (events-of fsm) (transitions-of fsm)))
    (setf (state-machine-of element) fsm))
  (setf (start-state-of fsm) (find-if (of-type 'start-state) (states-of fsm)))
  (setf (stop-states-of fsm) (collect-if (of-type 'stop-state) (states-of fsm))))

;; TODO: revive
#+nil
(define-copy-method (copy-one copy-query) ((state state) htable)
  state)

;; TODO: revive
#+nil
(define-copy-method (copy-one hu.dwim.perec::copy-shallow) ((state state) htable)
  state)

(def generic find-state (fsm state-name)
  (:method ((fsm-name symbol) (state-name symbol))
    (find-state (find-finite-state-machine fsm-name) state-name))

  (:method ((fsm finite-state-machine) (state-name symbol))
    (find state-name (states-of fsm) :key #'name-of)))

(def generic find-transition (fsm transition-name)
  (:method ((fsm-name symbol) (transition-name symbol))
    (find-transition (find-finite-state-machine fsm-name) transition-name))

  (:method ((fsm finite-state-machine) (transition-name symbol))
    (find transition-name (transitions-of fsm) :key #'name-of)))

(def generic find-event (fsm event-name)
  (:method ((fsm-name symbol) (event-name symbol))
    (find-event (find-finite-state-machine fsm-name) event-name))

  (:method ((fsm finite-state-machine) (event-name symbol))
    (find event-name (events-of fsm) :key #'name-of)))

(def (generic e) process-event (instance slot event &rest args)
  (:documentation "Process a single event and change the state according to the available transitions.")

  (:method ((instance standard-object) (slot-name symbol) (event-name symbol) &rest args)
    (bind ((class (class-of instance)))
      (apply #'process-event
             instance
             (find-slot class slot-name :otherwise (lambda () (error "Cannot find slot in ~A for name ~A" class slot-name)))
             event-name args)))

  ;; TODO: revive
  #+nil
  (:method ((instance standard-object) (state-property effective-property) (event-name symbol) &rest args)
    (bind ((fsm (state-machine-of state-property))
           (class (class-of instance))
           (state (slot-value-using-class class instance state-property))
           (event (find event-name (events-of fsm) :key #'name-of))
           (transition (find-if (lambda (transition)
                                  (and (eq (event-of transition) event)
                                       (eq (source-of transition) state)))
                                (transitions-of fsm))))
      (if transition
          (let ((new-state (target-of transition)))
            (hu.dwim.logger:standard-logger.debug "Firing transition ~A from finite state machine ~A for state property ~A in ~A" (name-of transition) (name-of fsm) state-property instance)
            (if (eq state new-state)
                (when (slot-boundp state 'on-stay-action)
                  (apply (on-stay-action-of state) args))
                (progn
                  (when (slot-boundp state 'on-leave-action)
                    (apply (on-leave-action-of state) args))
                  (when (slot-boundp transition 'action)
                    (apply (action-of transition) args))
                  (when (slot-boundp new-state 'on-enter-action)
                    (apply (on-enter-action-of new-state) args))
                  (setf (slot-value-using-class class instance state-property) new-state))))
          (error "Transition not found for ~A and ~A in ~A of ~A" state event fsm instance)))))

;; TODO: revive
#+nil
(def generic process-event* (instance event &rest args)
  (:method ((instance standard-object) (event-name symbol) &rest args)
    "Process a single event and change the state according to the currently available transitions. This variant searches for state-properties and fails if there's more than one."
    (let ((state-properties (collect-if (of-type 'state-property) (direct-properties-of (class-of instance)))))
      (if (= (length state-properties) 1)
          (apply 'process-event instance (slot-definition-name (first state-properties)) event-name args)
          (error "More than one state for ~A" instance)))))

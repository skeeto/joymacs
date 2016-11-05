;;; joydemo.el --- joystick calibration demo -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This demonstrates the functions in the joymacs package.

;;; Code:

(require 'cl-lib)
(require 'joymacs)

(defvar joydemo-buffer " *joydemo*"
  "Buffer used to display joystick calibration demo.")

(defvar-local joydemo-joystick nil
  "Current joystick state (handle buttons-vector axes-vector).")

(defvar-local joydemo-timer nil
  "Timer for gathering joystick events.")

(defun joydemo--grow (vec length)
  "Return VEC resized to LENGTH."
  (if (= length (length vec))
      vec
    (let ((new-vec (make-vector length nil)))
      (dotimes (i (min (length vec) length))
        (setf (aref new-vec i) (aref vec i)))
      new-vec)))

(define-derived-mode joydemo-mode special-mode "joystick"
  "Mode for joystick calibration demo, launched with `joydemo'."
  (let ((js (joymacs-open 0))
        (axes [])
        (buttons [])
        (event (make-vector 5 0)))
    (while (joymacs-read js event)
      (let ((type (aref event 1))
            (value (aref event 2))
            (number (aref event 3)))
        (cl-ecase type
          (:axis
           (when (>= number (length axes))
             (setf axes (joydemo--grow axes (1+ number))))
           (setf (aref axes number) value))
          (:button
           (when (>= number (length buttons))
             (setf buttons (joydemo--grow buttons (1+ number))))
           (setf (aref buttons number) value)))))
    (setf joydemo-joystick (list js buttons axes))
    (joydemo--draw buttons axes)
    (unless joydemo-timer
      (add-hook 'kill-buffer-hook #'joydemo--shutdown :local)
      joydemo-timer (run-at-time 0 (/ 1.0 30.0) #'joydemo--update))))

(defun joydemo ()
  "Launch the joystick calibration demo."
  (interactive)
  (with-current-buffer (get-buffer-create joydemo-buffer)
    (switch-to-buffer (current-buffer))
    (joydemo-mode)))

(defun joydemo--shutdown ()
  "Clean up resources used by the joystick calibration demo."
  (when joydemo-timer
    (cancel-timer joydemo-timer))
  (when joydemo-joystick
    (joymacs-close (car joydemo-joystick))
    (setf joydemo-joystick nil)))

(defun joydemo--draw (buttons axes)
  "Draw the given joystick state in the current buffer."
  (let ((inhibit-read-only t)
        (width (- (window-width) 3))
        (fill-column (window-width)))
    (erase-buffer)
    (dolist (axis (cl-coerce axes 'list))
      (let* ((left (floor (* width (/ (+ axis 1.0) 2.0))))
             (right (- width left)))
        (insert (propertize "[" 'face '((:weight bold))))
        (dotimes (_ left)
          (insert-char ?-))
        (insert (propertize "#" 'face 'font-lock-warning-face))
        (dotimes (_ right)
          (insert-char ?-))
        (insert (propertize "]\n\n" 'face '((:weight bold))))))
    (dolist (button (cl-coerce buttons 'list))
      (insert (if button
                  (propertize "X  " 'face 'font-lock-warning-face)
                (propertize "O  " 'face 'font-lock-comment-face))))
    (center-line)
    (newline)))

(defun joydemo--update ()
  "Timer function that reads joystick events and updates the state."
  (with-current-buffer (get-buffer-create joydemo-buffer)
    (when (get-buffer-window (current-buffer))
      (let ((event (make-vector 5 0))
            (event-count 0))
        (cl-destructuring-bind (js buttons axes) joydemo-joystick
          (while (joymacs-read js event)
            (cl-incf event-count)
            (let ((type (aref event 1))
                  (value (aref event 2))
                  (number (aref event 3)))
              (cl-ecase type
                (:axis   (setf (aref axes number) value))
                (:button (setf (aref buttons number) value)))))
          (when (> event-count 0)
            (joydemo--draw buttons axes)))))))

;;; joydemo.el ends here

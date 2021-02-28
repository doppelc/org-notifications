;;; org-notifications.el --- Creates notifications for org-mode entries -*- lexical-binding: t; -*-

;; Author: doppelc
;; URL: https://github.com/doppelc/org-notifications
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org "9.0") (sound-wav "0.02") (alert "0.1.0") (seq "2.21"))
;; Keywords: outlines

;; License: GNU GPLv3 (or later) <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; This package creates system notifications for org-agenda items with
;; timestamps. To start checking for notifications use
;; `org-notifications-start', and `org-notifications-stop' to stop checking.

;;; Code:

(require 'alert)
(require 'org-agenda)
(require 'seq)
(require 'sound-wav)

(defvar org-notifications-non-agenda-file (list "/path/to/file.org")
  "Add a file that's not ordinarily included among your agenda files.
For example when you want to use an org capture template to create \
notifications without clogging up your org agenda views.")

(defvar org-notifications-which-agenda-files 'agenda-only
  "Choose which agenda files should be checked for notifications. \
agenda-only: Use your agenda files from variable `org-agenda-files'.
non-agenda-file: Use only file(s) specified in \
`org-notifications-non-agenda-file'.
agenda-and-non-agenda-file: Use both your agenda files from \
variable `org-agenda-files' and files \
specified in `org-notifications-non-agenda-file'.")

(defvar org-notifications-title "Org-mode"
  "Title of the notifications. Headline is added as well.")

(defvar org-notifications-notify-before-time 300
  "Number of seconds before the event that the notification will be created.\
Use multiples of 60.")

(defvar org-notifications-agenda-tags-to-ignore nil
  "If non-nil, exclude entries with tags in the provided list.
Example: '(\"-tag1\" \"-tag2)")

(defvar org-notifications-agenda-tags-to-include nil
  "If non-nil, only look at entries with tags in the provided list \
to the exclusion of any other agenda entries.
Example: '(\"+tag1\" \"tag2)")

(defvar org-notifications-style 'libnotify
  "Set notification style. Check out the alert package for more styles.")

(defun org-notifications--set-style ()
  "Function to set the notification style for this package."
  (alert-add-rule
   :category "org-notifications"
   :style org-notifications-style))

(defvar org-notifications-play-sounds t
  "If non-nil, enable sounds with the notifications.")

(defvar org-notifications-sound "ding_elevator.wav"
  "One of the sounds from `org-notifications-sounds' or a path to a sound file.
E.g. `ding_elevator.wav' or `/path/to/file.wav'")

(defvar org-notifications--sounds
  '("ding_elevator.wav"
    "ding_percussion.wav")
   "Packaged sounds.")

(defvar org-notifications--sounds-directory
  (concat
   (file-name-directory (or load-file-name buffer-file-name))
   "sounds/")
  "Sound directory with packaged sounds.")

(defvar org-notifications--timer-object nil
  "Hold a reference to the timer object which periodically \
checks if any notification should be created.")



(defun org-notifications--get-events ()
  "Check for agenda events that are nearing and queues notifications for them."
  (save-window-excursion
    (save-restriction
    (let ((org-agenda-files (org-notifications--agenda-files))
          (org-agenda-use-time-grid nil)
          (org-agenda-skip-scheduled-if-done 't)
          (org-agenda-todo-keyword-format "")
          (org-agenda-remove-tags 't)
          (org-agenda-window-setup 'current-window)
          (org-agenda-skip-unavailable-files t)
          (org-agenda-sticky nil)
          (org-agenda-tag-filter-preset org-notifications-agenda-tags-to-ignore)
          (org-agenda-tag-filter-preset org-notifications-agenda-tags-to-include)
          (org-super-agenda-mode nil)
          (org-agenda-prefix-format '((agenda . "%-19t"))))
      (org-agenda-list 2))
    (dolist (times-and-headlines (mapcar 'org-notifications--extract-time-headline
                       (org-notifications--filter-agenda-no-time
                        (buffer-substring-no-properties
                         (point-min)
                         (point-max)))))
      (when (org-notifications--entry-within-interval-window (car times-and-headlines))
          (org-notifications--create-notify (cdr times-and-headlines) (car times-and-headlines))))
    (org-agenda-exit))))

(defun org-notifications--agenda-files ()
  "Handle which agenda files should be checked for notifications."
  (cond ((eq org-notifications-which-agenda-files 'agenda-only)
         org-agenda-files)
        ((eq org-notifications-which-agenda-files 'non-agenda-file)
         org-notifications-non-agenda-file)
        ((eq org-notifications-which-agenda-files 'agenda-and-non-agenda-file)
         (append org-agenda-files (list (car org-notifications-non-agenda-file))))))

(defun org-notifications--create-notify (headline time)
  "Queue a future notification `org-notifications-notify-before-time' \
seconds before scheduled time. \
HEADLINE is the org agenda entry's headline. \
TIME is the org agenda entry's time."
  (run-at-time
   (org-notifications--time-to-hhmm (time-subtract (org-notifications--hhmm-to-time time) org-notifications-notify-before-time)) nil #'org-notifications--notify headline))

(defun org-notifications--notify (headline)
  "Notify with a desktop notification and a sound, if enabled.
HEADLINE will appear on the notification."
  (alert headline
         :title org-notifications-title
         :category "org-notifications")
  (when org-notifications-play-sounds
      (org-notifications--play-sound)))

(defun org-notifications--hhmm-to-time (time)
  "Encode TIME such as `17:00' into a time value with today's date."
  (apply #'encode-time
         (parse-time-string
          (concat
           (format-time-string "%Y-%m-%d" (current-time)) " " time))))

(defun org-notifications--time-to-hhmm (time)
  "Format the TIME value to `HH:MM'."
  (format-time-string "%H:%M" time))

(defun org-notifications--play-sound ()
  "Play a sound from a file (full path) or from a file in \
`org-notifications--sounds-directory'."
  (if (member org-notifications-sound org-notifications--sounds)
      (sound-wav-play (concat org-notifications--sounds-directory org-notifications-sound))
    (sound-wav-play org-notifications-sound)))

(defun org-notifications--entry-within-interval-window (time)
  "Check how many seconds there are between scheduled items and current TIME.
Returns t if entry's time is close enough to create a notification."
  (let ((time-gap (time-to-seconds
                   (time-subtract
                    (org-notifications--hhmm-to-time time)
                    (current-time)))))
    (and
     (<= time-gap (+ org-notifications-notify-before-time org-notifications-notify-before-time))
     (>= time-gap org-notifications-notify-before-time))))

(defun org-notifications--extract-time-headline (agenda-line-as-string)
  "Create a cons cell containing the time `17:00' and the \
org agenda headline from AGENDA-LINE-AS-STRING. \
E.g.`TODO Do the dishes' of the agenda entry."
  (let ((time (car (split-string agenda-line-as-string "        ")))
        (headline (nth 1 (split-string agenda-line-as-string "        "))))
    (cons time headline)))

(defun org-notifications--filter-agenda-no-time (agenda-as-string)
  "Remove items without any time from AGENDA-AS-STRING, i.e. items \
without `17:00' and the like."
  (seq-filter
   (apply-partially #'string-match "[0-9]\\{0,2\\}:[0-9]\\{2\\}")
   (split-string agenda-as-string "\n")))

(defun org-notifications-start ()
  "Start the timer that is used to collect agenda items."
  (interactive)
  (org-notifications--set-style)
  (setq org-notifications--timer-object
        (run-at-time 1 org-notifications-notify-before-time 'org-notifications--get-events)))

(defun org-notifications-stop ()
  "Stop the timer that is used to collect agenda items."
  (interactive)
  (cancel-timer org-notifications--timer-object))

(provide 'org-notifications)

;;; org-notifications.el ends here

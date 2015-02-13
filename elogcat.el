;;; elogcat.el --- logcat interface for emacs

;; Copyright (C) 2015 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((s "1.9.0") (dash "2.10.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; logcat interface for emacs

;;; Code:
(require 's)
(require 'dash)

;;;; Declarations
(defvar elogcat-pending-output "")

(defface elogcat-verbose-face '((t (:foreground "DodgerBlue")))
  "Font Lock face used to highlight VERBOSE log records."
  :group 'elogcat)

(defface elogcat-debug-face '((t (:foreground "ForestGreen")))
  "Font Lock face used to highlight DEBUG log records."
  :group 'elogcat)

(defface elogcat-info-face '((t (:foreground "Gray45")))
  "Font Lock face used to highlight INFO log records."
  :group 'elogcat)

(defface elogcat-warning-face '((t (:foreground "Red")))
  "Font Lock face used to highlight WARN log records."
  :group 'elogcat)

(defface elogcat-error-face '((t (:foreground "Red" :bold t)))
  "Font Lock face used to highlight ERROR log records."
  :group 'elogcat)

(defvar elogcat-face-alist
  '(("V" . elogcat-verbose-face)
    ("D" . elogcat-debug-face)
    ("I" . elogcat-info-face)
    ("W" . elogcat-warning-face)
    ("E" . elogcat-error-face)))

(defcustom elogcat-option-list
  (list "logcat"
        "-v" "threadtime"
        "-b" "main"
        "-b" "events"
        "-b" "system"
        "-b" "radio"
        )
  "logcat options"
  :type '(repeat string)
  :group 'elogcat)

(defvar elogcat-include-filter-regexp "")
(defvar elogcat-exclude-filter-regexp "")

(defconst elogcat-process-name "elogcat"
  "Name of elogcat process")

(defcustom elogcat-buffer "*elogcat*"
  "Name for elogcat buffer"
  :type 'string
  :group 'elogcat)

(defcustom elogcat-mode-line '(:eval (elogcat-mode-line-status-text))
  "Mode line lighter for elogcat"
  :group 'elogcat
  :type 'sexp
  :risky t
  :package-version '(elogcat . "0.1.0"))

(defun get-mode (option)
  (if (-contains? elogcat-option-list option)
      (s-word-initials option)
    "-"))

(defun elogcat-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line."
  (concat " elogcat["
          (get-mode "main")
          (get-mode "system")
          (get-mode "events")
          (get-mode "radio")
          "]"))

(defun elogcat-erase-buffer ()
  "Clear elogcat buffer"
  (interactive)
  (with-current-buffer elogcat-buffer
    (let ((buffer-read-only nil))
      (erase-buffer)))
  (apply 'start-process
         "elogcat-clear"
         "*elogcat-clear*"
         "adb"
         (-concat elogcat-option-list '("-c")))
  (sleep-for 1)
  (elogcat-stop)
  (elogcat))

(defun elogcat-clear-filter ()
  "Clear the filter"
  (interactive)
  (elogcat-set-filter ""))

(defun elogcat-set-filter (regexp-filter)
  "Set the filter"
  (interactive "MRegexp Filter: ")
  (with-current-buffer elogcat-buffer
    (let ((buffer-read-only nil)
          (info-face (cdr (assoc "I" elogcat-face-alist)))
          msg)
      (goto-char (point-max))
      (if (equal (length regexp-filter) 0)
          (setq msg "\n\n*** Filter is cleared ***\n\n")
        (setq msg (concat "\n\n*** Filter is changed to '" regexp-filter
                          "' ***\n\n")))
      (insert (propertize msg 'font-lock-face info-face))))
  (setq elogcat-include-filter-regexp regexp-filter))

(defun elogcat-process-filter (process output)
  "Process filter"
  (with-current-buffer elogcat-buffer
    (let ((following (= (point-max) (point)))
          (buffer-read-only nil)
          (pos 0)
          (output (concat elogcat-pending-output
                          (replace-regexp-in-string "" "" output))))
      (save-excursion
        (while (string-match "\n" output pos)
          (let ((line (substring output pos (match-beginning 0))))
            (setq pos (match-end 0))
            (goto-char (point-max))
            (if (not (string-match "[Aa]udio" line))
                (when (string-match elogcat-include-filter-regexp line)
                  (let* ((log-list (s-split-up-to "\s+" line 6))
                         (level (nth 4 log-list))
                         (level-face (cdr (or (assoc level elogcat-face-alist)
                                              (assoc "I" elogcat-face-alist)))))
                    (insert (propertize line 'font-lock-face level-face))
                    (insert "\n"))))))
        (setq elogcat-pending-output (substring output pos)))
      (when following (goto-char (point-max))))))

(defmacro elogcat-define-switch-option (sym option)
  "Define function"
  (let ((fun (intern (format "elogcat-switch-%s" sym)))
        (doc (format "Switch to %s" option)))
    `(progn
       (defun ,fun () ,doc
              (interactive)
              (-if-let (num (-elem-index ,option elogcat-option-list))
                  (setq elogcat-option-list
                        (-remove-at-indices (list (- num 1) num) elogcat-option-list))
                (setq elogcat-option-list (-concat elogcat-option-list '("-b" ,option))))
              (elogcat-stop)
              (elogcat)))))

(elogcat-define-switch-option events "events")
(elogcat-define-switch-option system "system")
(elogcat-define-switch-option main "main")
(elogcat-define-switch-option radio "radio")

(defvar elogcat-mode-map nil
  "Keymap for elogcat minor mode.")

(unless elogcat-mode-map
  (setq elogcat-mode-map (make-sparse-keymap)))

(--each '(("C" . elogcat-erase-buffer)
          ("f" . elogcat-set-filter)
          ("F" . occur)
          ("c" . elogcat-clear-filter)
          ("q" . elogcat-delete-window)
          ("e" . elogcat-switch-events)
          ("s" . elogcat-switch-system)
          ("m" . elogcat-switch-main)
          ("r" . elogcat-switch-radio))
  (define-key elogcat-mode-map (read-kbd-macro (car it)) (cdr it)))

(define-minor-mode elogcat-mode
  "Minor mode for elogcat"
  :lighter elogcat-mode-line
  nil " elogcat" elogcat-mode-map)

(defun elogcat-stop ()
  "Stop the adb logcat process"
  (-when-let (process (get-process "elogcat"))
    (delete-process process)))

(defun elogcat ()
  "Start the adb logcat process"
  (interactive)
  (when (not (get-process "elogcat"))
    (apply 'start-process
     "elogcat"
     elogcat-buffer
     "adb"
     elogcat-option-list)
    (set-process-filter (get-process "elogcat") 'elogcat-process-filter)
    (with-current-buffer elogcat-buffer
      (elogcat-mode t)
      (setq buffer-read-only t)
      (font-lock-mode t))
    (switch-to-buffer elogcat-buffer)
    (goto-char (point-max))))

(provide 'elogcat)
;;; elogcat.el ends here

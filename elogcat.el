;;; elogcat.el --- logcat interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.3.0
;; Keywords: tools
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

;; logcat interface for Emacs

;;; Code:
(require 's)
(require 'dash)
(require 'cl-lib)

;;;; Declarations
(defvar elogcat-pending-output "")

(defgroup elogcat nil
  "Interface with elogcat."
  :group 'external)

(defface elogcat-verbose-face '((t (:inherit default)))
  "Font Lock face used to highlight VERBOSE log records."
  :group 'elogcat)

(defface elogcat-debug-face '((t (:inherit font-lock-preprocessor-face)))
  "Font Lock face used to highlight DEBUG log records."
  :group 'elogcat)

(defface elogcat-info-face '((t (:inherit success)))
  "Font Lock face used to highlight INFO log records."
  :group 'elogcat)

(defface elogcat-warning-face '((t (:inherit warning)))
  "Font Lock face used to highlight WARN log records."
  :group 'elogcat)

(defface elogcat-error-face '((t (:inherit error)))
  "Font Lock face used to highlight ERROR log records."
  :group 'elogcat)

(defface elogcat-fatal-face '((t (:inherit error)))
  "Font Lock face used to highlight FATAL log records."
  :group 'elogcat)

(defvar elogcat-face-alist
  '(("V" . elogcat-verbose-face)
    ("D" . elogcat-debug-face)
    ("I" . elogcat-info-face)
    ("W" . elogcat-warning-face)
    ("E" . elogcat-error-face)
    ("F" . elogcat-fatal-face)))

(defconst elogcat-level-priority '("V" "D" "I" "W" "E" "F")
  "Log levels in ascending priority order.")

(defcustom elogcat-logcat-command
  "logcat -v threadtime -b main -b system -b radio -b events -b crash -b kernel"
  "Logcat command."
  :group 'elogcat
  :type 'string)

(defcustom elogcat-default-tail 1
  "Default number of historical lines to show with `-T'.
Set to nil to replay the full ring buffer by default."
  :group 'elogcat
  :type '(choice (const :tag "Full history" nil)
                 (integer :tag "Number of lines")))

(defvar elogcat-include-filter-regexp nil)
(defvar elogcat-exclude-filter-regexp nil)
(defvar elogcat-min-level "V"
  "Minimum log level to display.  One of V D I W E F.")
(defvar elogcat-package-filter nil
  "Current package filter string shown in mode line, e.g. \"com.example:1234\".")

(defconst elogcat-process-name "elogcat")

(defcustom elogcat-buffer "*elogcat*"
  "Name for elogcat buffer."
  :group 'elogcat
  :type 'string)

(defcustom elogcat-mode-line '(:eval (elogcat-make-status))
  "Mode line lighter for elogcat."
  :group 'elogcat
  :type 'sexp
  :risky t
  :package-version '(elogcat . "0.2.0"))

(defun elogcat-get-log-buffer-status (buffer)
  "Get a log buffer status by BUFFER."
  (let ((end (if (string= buffer "kernel") "" "|")))
    (if (s-contains? buffer elogcat-logcat-command)
        (concat (s-word-initials buffer) end)
      (concat "-" end))))

(defun elogcat-make-status (&optional _status)
  "Get a log buffer status for use in the mode line."
  (format " elogcat[%s]%s<%s>"
          (mapconcat #'elogcat-get-log-buffer-status
                     '("main" "system" "radio" "events" "crash" "kernel") "")
          (if elogcat-package-filter
              (format "(%s)" elogcat-package-filter)
            "")
          elogcat-min-level))

(defun elogcat-erase-buffer ()
  "Clear elogcat buffer."
  (interactive)
  (with-current-buffer elogcat-buffer
    (let ((buffer-read-only nil))
      (erase-buffer)))
  (start-process-shell-command "elogcat-clear"
                               "*elogcat-clear*"
                               (concat "adb " elogcat-logcat-command " -c"))
  (sleep-for 1)
  (elogcat-stop)
  (elogcat))

(defun elogcat-clear-filter (filter)
  "Clear the FILTER."
  (interactive)
  (let ((buffer-read-only nil))
    (goto-char (point-max))
    (insert (propertize
             (concat "--------- " (symbol-name filter) " is cleared\n")
             'face (cdr (assoc "V" elogcat-face-alist))))
    (set filter nil)))

(defun elogcat-clear-include-filter ()
  "Clear the include filter."
  (interactive)
  (elogcat-clear-filter 'elogcat-include-filter-regexp))

(defun elogcat-clear-exclude-filter ()
  "Clear the exclude filter."
  (interactive)
  (elogcat-clear-filter 'elogcat-exclude-filter-regexp))

(defun elogcat-set-filter (regexp filter)
  "Set the filter to REGEXP FILTER."
  (with-current-buffer elogcat-buffer
    (let ((buffer-read-only nil)
          (info-face (cdr (assoc "V" elogcat-face-alist))))
      (goto-char (point-max))
      (insert (propertize
               (concat "--------- " (symbol-name filter) " '" regexp "'\n")
               'face info-face))
      (set filter regexp))))

(defun elogcat-show-status ()
  "Show current status."
  (interactive)
  (let ((buffer-read-only nil))
    (goto-char (point-max))
    (insert (propertize
             (concat "--------- "
                     "Include: '" elogcat-include-filter-regexp "' , "
                     "eXclude: '" elogcat-exclude-filter-regexp "' , "
                     "Level: '" elogcat-min-level "'\n")
             'face (cdr (assoc "V" elogcat-face-alist))))))

(defun elogcat-set-include-filter (regexp)
  "Set the REGEXP for include filter."
  (interactive "MRegexp Include Filter: ")
  (elogcat-set-filter regexp 'elogcat-include-filter-regexp))

(defun elogcat-set-exclude-filter (regexp)
  "Set the REGEXP for exclude filter."
  (interactive "MRegexp Exclude Filter: ")
  (elogcat-set-filter regexp 'elogcat-exclude-filter-regexp))

(defun elogcat-set-level (level)
  "Set minimum log LEVEL filter.
Only lines at or above this level will be displayed."
  (interactive
   (list (completing-read
          (format "Min level (current: %s): " elogcat-min-level)
          '("V - Verbose" "D - Debug" "I - Info"
            "W - Warning" "E - Error" "F - Fatal")
          nil t)))
  (setq elogcat-min-level (substring level 0 1))
  (message "elogcat: min level set to %s" elogcat-min-level))

(defun elogcat-process-filter (process output)
  "Adb PROCESS make line from OUTPUT buffer.
Batch-inserts all matching lines in one operation for speed.
Only auto-scrolls windows whose point was already at the tail;
if the user scrolled away the window stays put."
  (when (get-buffer elogcat-buffer)
    (with-current-buffer elogcat-buffer
      (let* ((old-max (point-max))
             (following-wins
              (cl-loop for win in (get-buffer-window-list elogcat-buffer nil t)
                       when (>= (window-point win) old-max)
                       collect win))
             (gc-cons-threshold most-positive-fixnum)
             (inhibit-redisplay t)
             (buffer-read-only nil)
             (raw (concat elogcat-pending-output output))
             (output (if (string-search "\r" raw)
                         (string-replace "\r" "" raw)
                       raw))
             (include elogcat-include-filter-regexp)
             (exclude elogcat-exclude-filter-regexp)
             (min-level-idx (or (cl-position elogcat-min-level
                                             elogcat-level-priority
                                             :test #'string=) 0))
             (pos 0)
             (chunks nil))
        (while (string-match "\n" output pos)
          (let* ((end (match-beginning 0))
                 (line (substring output pos end)))
            (setq pos (match-end 0))
            (when (and (or (null include) (string-match-p include line))
                       (or (null exclude) (not (string-match-p exclude line))))
              (let ((formatted
                     (if (string-match
                          "^[0-9][0-9]-[0-9][0-9] +[0-9:.]+  *[0-9]+  *[0-9]+ \\([VDIWEF]\\) "
                          line)
                         (let* ((lvl (match-string 1 line))
                                (face (cdr (or (assoc lvl elogcat-face-alist)
                                               (assoc "I" elogcat-face-alist)))))
                           (when (>= (or (cl-position lvl elogcat-level-priority
                                                      :test #'string=) 0)
                                     min-level-idx)
                             (propertize (concat line "\n") 'face face)))
                       (concat line "\n"))))
                (when formatted
                  (push formatted chunks))))))
        (setq elogcat-pending-output (substring output pos))
        (when chunks
          (save-excursion
            (goto-char (point-max))
            (insert (apply #'concat (nreverse chunks)))))
        (let ((new-max (point-max)))
          (dolist (win following-wins)
            (set-window-point win new-max)))))))

(defun elogcat-process-sentinel (process event)
  "Test PROCESS EVENT.")

(defmacro elogcat-define-toggle-function (sym ring-buffer-name)
  "Define a function with SYM and RING-BUFFER-NAME."
  (let ((fun (intern (format "elogcat-toggle-%s" sym)))
        (doc (format "Switch to %s" ring-buffer-name)))
    `(progn
       (defun ,fun () ,doc
              (interactive)
              (let ((option (concat "-b " ,ring-buffer-name)))
                (if (s-contains? option elogcat-logcat-command)
                    (setq elogcat-logcat-command
                          (mapconcat (lambda (args) (concat (s-trim args)))
                                     (s-split option elogcat-logcat-command) " "))
                  (setq elogcat-logcat-command
                        (s-concat (s-trim elogcat-logcat-command) " " option))))
              (let ((buffer-read-only nil))
                (erase-buffer))
              (elogcat-stop)
              (elogcat)))))

(elogcat-define-toggle-function main "main")
(elogcat-define-toggle-function system "system")
(elogcat-define-toggle-function radio "radio")
(elogcat-define-toggle-function events "events")
(elogcat-define-toggle-function crash "crash")
(elogcat-define-toggle-function kernel "kernel")

(defvar elogcat-mode-map nil
  "Keymap for elogcat minor mode.")

(unless elogcat-mode-map
  (setq elogcat-mode-map (make-sparse-keymap)))

(--each '(("C" . elogcat-erase-buffer)
          ("i" . elogcat-set-include-filter)
          ("x" . elogcat-set-exclude-filter)
          ("I" . elogcat-clear-include-filter)
          ("X" . elogcat-clear-exclude-filter)
          ("L" . elogcat-set-level)
          ("P" . elogcat-toggle-package)
          ("S" . elogcat-save-buffer)
          ("g" . elogcat-show-status)
          ("F" . occur)
          ("q" . elogcat-exit)
          ("m" . elogcat-toggle-main)
          ("s" . elogcat-toggle-system)
          ("r" . elogcat-toggle-radio)
          ("e" . elogcat-toggle-events)
          ("c" . elogcat-toggle-crash)
          ("k" . elogcat-toggle-kernel))
  (define-key elogcat-mode-map (read-kbd-macro (car it)) (cdr it)))

(define-minor-mode elogcat-mode
  "Minor mode for elogcat."
  :lighter elogcat-mode-line
  nil " elogcat" elogcat-mode-map)

(defun elogcat-exit ()
  "Exit elogcat."
  (interactive)
  (let* ((buf (current-buffer))
         (proc (get-buffer-process buf)))
    (when (process-live-p proc)
      (kill-process proc)
      (sleep-for 0.1))
    (kill-buffer buf)))

(defun elogcat-toggle-package (pkg)
  "Toggle filtering logcat output by Android package PKG.
Interactively, select from installed third-party packages.
If the selected package is already filtered, remove the filter."
  (interactive
   (list (completing-read
          "Select package: "
          (mapcar (lambda (name)
                    (replace-regexp-in-string "package:" "" name))
                  (split-string
                   (string-trim
                    (shell-command-to-string "adb shell pm list package -3"))
                   "\n")))))
  (let ((pid (string-trim
              (shell-command-to-string (concat "adb shell pidof " pkg))))
        (option " --pid="))
    (when (string-empty-p pid)
      (error "App %s is not running" pkg))
    (if (string-match (format "\\(%s\\)\\([0-9]*\\)"
                              (regexp-quote option))
                      elogcat-logcat-command)
        (setq elogcat-logcat-command
              (if (string= (match-string 2 elogcat-logcat-command) pid)
                  (progn (setq elogcat-package-filter nil)
                         (replace-match "" nil nil elogcat-logcat-command))
                (setq elogcat-package-filter (format "%s:%s" pkg pid))
                (replace-match pid nil nil elogcat-logcat-command 2)))
      (setq elogcat-logcat-command (concat elogcat-logcat-command option pid))
      (setq elogcat-package-filter (format "%s:%s" pkg pid))))
  (let ((buffer-read-only nil))
    (erase-buffer))
  (elogcat-stop)
  (elogcat))

(defun elogcat-stop ()
  "Stop the adb logcat process."
  (-when-let (proc (get-process "elogcat"))
    (delete-process proc)))

;;;###autoload
(defun elogcat (&optional arg)
  "Start the adb logcat process.
Without prefix, show the last `elogcat-default-tail' lines then stream.
With numeric prefix N, show the last N lines then stream.
With bare \\[universal-argument], replay full ring buffer history."
  (interactive "P")
  (unless (get-process "elogcat")
    (let* ((tail-arg (cond
                      ((consp arg) "")
                      (arg (format " -T %d" (prefix-numeric-value arg)))
                      (elogcat-default-tail
                       (format " -T %d" elogcat-default-tail))
                      (t "")))
           (cmd (concat elogcat-logcat-command
                        (unless (s-contains? "-b" elogcat-logcat-command)
                          " -s")
                        tail-arg))
           (proc (start-process-shell-command
                  "elogcat" elogcat-buffer
                  (concat "adb shell " (shell-quote-argument cmd)))))
      (set-process-filter proc #'elogcat-process-filter)
      (set-process-sentinel proc #'elogcat-process-sentinel)
      (with-current-buffer elogcat-buffer
        (elogcat-mode t)
        (setq buffer-read-only t)
        (font-lock-mode -1))
      (switch-to-buffer elogcat-buffer)
      (goto-char (point-max)))))

(defun elogcat-save-buffer ()
  "Save the current elogcat buffer to a file and stop the logcat process."
  (interactive)
  (save-buffer)
  (elogcat-stop))

(provide 'elogcat)
;;; elogcat.el ends here

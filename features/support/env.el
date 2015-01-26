(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq elogcat-root-path project-directory))

(add-to-list 'load-path elogcat-root-path)

(require 'elogcat)
(require 'espuds)
(require 'ert)

(Before
 (switch-to-buffer
  (get-buffer-create "*elogcat*"))
 (erase-buffer)
 (fundamental-mode)
 (deactivate-mark))

(After)

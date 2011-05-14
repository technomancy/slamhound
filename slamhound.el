;;; slamhound.el --- Rip Clojure namespaces apart and rebuild them.

;; Copyright (C) 2011 Phil Hagelberg
;;
;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://github.com/technomancy/slamhound
;; Version: 1.0.0
;; Keywords: tools, lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Destroys the ns form of a clojure-mode buffer and attempts to
;; rebuild it by searching the classpath. Requires an active slime
;; connection.

;; M-x slamhound operates on the current buffer

;; M-x slamhound-project operates on the current source tree

;; If the namespace cannot be reconstructed for whatever reason, the
;; file will remain untouched and the reason will be shown.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'slime)

;; Single-file:

(defun slamhound-clj-string (filename)
  (format "%s" `(do (require 'slam.hound)
                    (try (print (.trim (slam.hound/reconstruct
                                        ,(format "\"%s\"" filename))))
                     (catch Exception e
                            (println :error (.getMessage e)))))))

(defun slamhound-point-at-end-of-sexp (start)
  (save-excursion (goto-char start) (end-of-sexp) (point)))

(defun slamhound-prettify-subclause ()
  (backward-char 2)
  (let ((beginning-of-sexp (point)))
    (while (search-forward-regexp "[ \n]" (slamhound-point-at-end-of-sexp
                                           beginning-of-sexp) t)
      (just-one-space -1))))

(defun slamhound-prettify-clause (clause)
  (just-one-space -1)
  (beginning-of-line)
  (let ((end-of-clause (save-excursion (end-of-sexp) (point))))
    (while (search-forward-regexp "[(\\[][a-z]" end-of-clause t)
      (slamhound-prettify-subclause))))

(defun slamhound-prettify ()
  (interactive)
  (goto-char (point-min))
  (let ((end-of-ns (save-excursion (end-of-defun) (point))))
    (while (search-forward-regexp "(:\\([-a-z]+\\)" end-of-ns t)
      (slamhound-prettify-clause (match-string 0)))
    (indent-region (point-min) end-of-ns)))

;;;###autoload
(defun slamhound ()
  "Run slamhound on the current buffer. Requires active slime connection."
  (interactive)
  (let* ((code (slamhound-clj-string buffer-file-name))
         (result (first (slime-eval `(swank:eval-and-grab-output ,code)))))
    (setq rrr result)
    (if (string-match "^:error \\(.*\\)" result)
        (error (match-string 1 result))
      (goto-char (point-min))
      (kill-sexp)
      ;; TODO: translate \n into newline
      (insert result)
      (slamhound-prettify))))

;; Project-wide:

(defun slamhound-project-files (project-root)
  (split-string (shell-command-to-string
                 (format "find %s -name \"*.clj\"" project-root))))

(defun slamhound-track-failures (failures file)
  (condition-case failure
      (save-excursion
        (find-file file)
        (slamhound)
        failures)
    (error (cons (cons file (cadr failure)) failures))))

;;;###autoload
(defun slamhound-project ()
  "Run slamhound on an entire project. Experimental."
  (interactive)
  (save-some-buffers)
  (save-excursion
    (let* ((root (locate-dominating-file default-directory "src"))
           (files (slamhound-project-files (format "%s/src" root)))
           (errors (reduce 'slamhound-track-failures files :initial-value nil)))
      (setq eee errors)
      (if errors
          (message "Couldn't slam %s." errors)
          (message "All namespaces successfully slammed.")))))

(provide 'slamhound)
;;; slamhound.el ends here


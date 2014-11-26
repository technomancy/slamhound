;;; slamhound.el --- Rip Clojure namespaces apart and rebuild them.

;; Copyright © 2011-2012 Phil Hagelberg
;;
;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: https://github.com/technomancy/slamhound
;; Version: 2.1.0
;; Keywords: tools, lisp

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Destroys the ns form of a clojure-mode buffer and attempts to
;; rebuild it by searching the classpath. Requires an active
;; connection to either slime or nrepl.

;; M-x slamhound

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

(or (require 'nrepl-client nil t)
    (require 'nrepl nil t)
    (error "Please install either the nrepl.el or cider package."))

(defun slamhound-clj-string (filename)
  (format "%s" `(do (require 'slam.hound)
                    (try (print (.trim (slam.hound/reconstruct
                                        ,(format "\"%s\"" filename))))
                     (catch Exception e
                            (println :error (.getMessage e)))))))

;;;###autoload
(defun slamhound ()
  "Run slamhound on the current buffer.

  Requires active nrepl or slime connection."
  (interactive)
  (let* ((code (slamhound-clj-string buffer-file-name))
         (result (nrepl-send-string-sync code))
         (out (if (plist-member result :stdout)
                  (plist-get result :stdout)
                (lax-plist-get (cdr result) "out"))))
    (if (string-match "^:error \\(.*\\)" out)
        (error (match-string 1 out))
      (goto-char (point-min))
      ;; skip any header comments before the ns
      (forward-sexp)
      (backward-kill-sexp)
      (insert out))))

(provide 'slamhound)
;;; slamhound.el ends here

;;; slamhound.el --- Rip Clojure namespaces apart and rebuild them. -*- lexical-binding: t -*-

;; Copyright © 2011-2012 Phil Hagelberg
;;
;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: https://github.com/technomancy/slamhound
;; Package-Version: 20140506.1618
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

(unless (require 'cider-interaction nil t)
  (error "Please install CIDER package."))

(defalias '-> 'thread-first)


(defun slamhound-clj-string (filename)
  "Return a snippet of Clojure code as a string, which, when
executed, will do the slamhound reconstruction on a given
`filename'. The snippet itself should also return a string,
either a new `ns' form or description of an error."
  (-> (pp-to-string
       `(do (require 'slam.hound)
            (try
             (-> ,filename
               slam.hound/reconstruct
               .trim)
             (catch Exception e
               (format ":error %s" (.getMessage e))))))
    ;; pp-to-string escapes dots in symbols with two slashes, which Clojure
    ;; reader will not recognize. So we remove them.
    (replace-in-string "\\\\" "")))

(require 'cider-mode)

(defun good-resultp (result)
  (and result (not (string-prefix-p ":error" result))))

;;;###autoload
(defun slamhound ()
  "Run slamhound on the current buffer. Slamhound is a tool for
automatically managing `ns' forms in Clojure files. It can detect
when you're trying to use something that wasn't properly required
and add the corresponding require/import into the `ns'. It can
also detect when something is imported but unused, in which case
it clears he corresponding require/import from the `ns' form.

Requires active CIDER connection."
  (interactive)
  (let* ((fname (concat buffer-file-name "1"))
         (code (slamhound-clj-string fname))
         (buf (current-buffer))
         (ns-data (buffer-substring-no-properties (point-min)
                                                  (point-max))))
    ;; Save buffer contents to a temporary file. Clojure will read the code from
    ;; there.
    (with-temp-file fname (insert ns-data))
    (-> code
      (cider-nrepl-request:eval
       (lambda (result)
         ;; This callback is called three times, with correct result the first
         ;; time and some garbage results for the latter two calls.
         (when (f-exists? fname)
           (delete-file fname)
           (setq result (nrepl-dict-get result "value"))
           (if (good-resultp result)
               (with-current-buffer buf
                 (save-excursion
                   (goto-char (point-min))
                   (forward-sexp)       ; skips any header comments before the ns
                   (backward-kill-sexp)
                   ;; `result' is a string literal with some characters escaped,
                   ;; we need to clear these escapes before inserting result
                   ;; into the buffer
                   (-> result
                     (replace-in-string "\\\\n" "\n")
                     (replace-in-string "\\\"" "")
                     (insert))))
             (message "Something went wrong: %s" result))))))))


(provide 'slamhound)
;;; slamhound.el ends here

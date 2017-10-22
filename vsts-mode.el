;;; vsts-mode.el --- Vsts mode                       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  jtbm37

;; Author: jtbm37
;; Keywords: extensions

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

;; 

;;; Code:

(require 'bui)

(defgroup vsts nil
  "Major mode for vsts."
  :prefix "vsts-")

(defcustom vsts-instance nil
  "VS Team Services account name"
  :type 'string
  :group 'vsts)

(defcustom vsts-api-version "2.0"
  "API version to use"
  :type 'string
  :group 'vsts)

(defcustom vsts-project nil
  "VS Team Services project name"
  :type 'string
  :group 'vsts)

(defcustom vsts-token nil
  "VS Team Services authorization token"
  :type 'string
  :group 'vsts)

(defcustom vsts-repository nil
  "VS Team Services current repository"
  :type 'string
  :group 'vsts)


(defconst vsts-base-uri "https://%s.visualstudio.com/DefaultCollection/")
(defconst vsts-workitems-api "_apis/wit/WorkItems")
(defconst vsts-builds-api "_apis/build/builds")
(defconst vsts-build-definitions-api "_apis/build/definitions")

(defun vsts/get-url (area &optional project-p api-version)
  (let ((project (when project-p
		   (concat vsts-project "/")))
	(version (or api-version vsts-api-version)))
    (concat (format vsts-base-uri vsts-instance) project area (format "?api-version=%s" version))))

(defun vsts--submit-request (url callback &optional type params async)
  (if (require 'request nil 'noerror)
      (lexical-let* ((c callback)
		     (type (or type "POST"))
		     (token (concat  "Basic " vsts-token)))
        (request url
                 :type type
		 :headers `(("Authorization" . ,token)
			    ("Content-Type" . "application/json"))
                 :parser 'json-read
                 :sync (not async)
                 :data (json-encode params)
                 :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                       (message "Error from %s : %S" url error-thrown)))
                 :complete
                 (lambda (&rest _)
                   (when omnisharp-debug
                     (message "Request completed")))
                 :success (cl-function (lambda (&key data &allow-other-keys)
                                         (progn
                                           (when c
                                             (funcall c data))
                                           (when omnisharp-debug
                                             (message "Request succeeded"))
                                           )))
                 :status-code '((404 . (lambda (&rest _) (message (format "Endpoint %s does not exist." url))))
                                (500 . (lambda (&rest _) (message (format "Error from  %s." url))))
                                )))
    (message "ERROR: You must install 'request-deferred' package")))

(defun vsts/display-items (items)
  (message "Items: %s" items))

(defvar vsts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'vsts/load-items)
    map)
  "Keymap for vsts mode.")

(provide 'vsts-mode)
;;; vsts-mode.el ends here

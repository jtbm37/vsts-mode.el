;;; vsts-mode.el --- Vsts mode                       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  jtbm37

;; Author: jtbm37
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (bui "1.1.0") (dash "2.13.0")
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

(require 'request)
(require 'bui)
(require 'vsts-builds)
(require 'dash)

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
(defconst vsts-builds-api "_apis/build/builds")
(defconst vsts-build-definitions-api "_apis/build/definitions")

(defun vsts/set-credentials ()
  "Sets all necessary parameters for querying vsts. The values are fetched from .authinfo.gpg"
  (unless vsts-token
    (-if-let* ((entry (nth 0 (auth-source-search :max 1 :host "vsts")))
	       (login (plist-get entry :user))
	       (project (plist-get entry :project))
	       (repo (plist-get entry :repo))
	       (secretfun (plist-get entry :secret))
	       (secret (funcall secretfun)))
	(progn
	  (setq vsts-instance login
		vsts-token secret
		vsts-project project
		vsts-repository repo))
      (user-error "VSTS credentials not found. Please add a `vsts' entry into your .authinfo.gpg file."))))

(defun vsts/get-web-url (area)
  "Returns the base url for the web interface"
  (concat (format "https://%s.visualstudio.com/%s" vsts-instance vsts-project) area))

(defun vsts/get-url (area &optional project-p api-version)
  (let ((project (when project-p
		   (concat vsts-project "/")))
	(version (or api-version vsts-api-version)))
    (concat (format vsts-base-uri vsts-instance) project area (format "?api-version=%s" version))))

(defun vsts--submit-request (url callback &optional type params async)
  (if (require 'request nil 'noerror)
      (lexical-let* ((c callback)
		     (type (or type "POST"))
		     (token (concat  "Basic " (base64-encode-string (concat ":" vsts-token))))
		     (c-type (if (string-equal type "PATCH")
				 "application/json-patch+json"
			       "application/json")))
        (request url
                 :type type
		 :headers `(("Authorization" . ,token)
			    ("Content-Type" . ,c-type))
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

(defun vsts/open-item ()
  "Opens current item's url in list"
  (interactive)
  (when-let ((bui-buffer-type (bui-current-buffer-type))
	     (url (cond ((equal bui-buffer-type 'list)
			 (alist-get 'url (bui-list-current-entry)))
			((equal bui-buffer-type 'info)
			 (alist-get 'url (bui-current-args)))
			(t nil))))
    (browse-url url)))

(defvar vsts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'vsts/load-items)
    map)
  "Keymap for vsts mode.")

;;;###autoload
(defun vsts/show-builds ()
  "Shows list of builds in `vsts-project'"
  (interactive)
  ;; this will be moved in the main screen when done
  ;; For now this is the main entry point of the app
  (vsts/set-credentials)
  (bui-get-display-entries 'builds 'list))

(provide 'vsts-mode)
;;; vsts-mode.el ends here

;;; vsts-builds.el --- Builds api                    -*- lexical-binding: t; -*-

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

;; Api to list vsts builds

;;; Code:

(require 'vsts-git)
(require 'vsts-build-definitions)

(defconst vsts-build-artifacts-api "_apis/build/builds/%s/artifacts")
(defconst vsts-build-artifact-content-api "_apis/resources/Containers")

(defvar vsts-builds nil
  "Contains all builds for the builds page")

(defun vsts/get-builds ()
  "Displays all builds"
  (let ((base-url (vsts/get-url vsts-builds-api t))
	(params "&$top=10"))
    (vsts--submit-request (concat base-url params) '(lambda (data)
						      (setq vsts-builds (vsts/parse-builds data))) "GET" nil nil)
    vsts-builds))

(defun vsts/parse-build-item (data)
  "Returns alist of some of the build fields"
  (let ((branch (alist-get 'sourceBranch data))
	(finish-at (alist-get 'finishTime data))
	(data-result (alist-get 'result data))
	build)
    (if (string= data-result "canceled")
	(push (cons 'status "canceled") build)
      (push (assoc 'status data) build))
    (push (cons 'result (when finish-at (if (equal data-result "succeeded") "✓" "✗"))) build)
    (push (assoc 'buildNumber data) build)
    (setq branch (replace-regexp-in-string "refs/pull/\\([0-9]*?\\)/merge" "PR\\1" branch nil nil 0))
    (setq branch (replace-regexp-in-string "refs/heads/" "" branch))
    (push (cons 'sourceBranch branch) build)
    (push (cons 'sourceVersion (substring (alist-get 'sourceVersion data) 0 8)) build)
    (push (assoc 'name (assoc 'definition data)) build)
    (push (cons 'requestedBy (alist-get 'displayName (alist-get 'requestedBy data))) build)
    (push (cons 'completed (time-to-ago finish-at)) build)
    (push (assoc 'id data) build)))

(defun time-to-ago (datetime)
  "Returns how long ago `datetime' was."
  (when datetime
    (let ((ago (time-to-seconds (time-since (date-to-time datetime)))))
      (cond ((and (> ago 60) (< ago 3600))
	     (format "%s minutes ago" (round (/ ago 60))))
	    ((and (> ago 3600) (< ago 86400))
	     (format "%s hours ago" (round (/ ago 3600))))
	    (t
	     (format "%s days ago" (round (/ ago 86400))))))))

(defun vsts/parse-builds (response)
  "Reads response and returns a list of builds in `'response"
  (let ((values (cdr (cadr response)))
	result)
    (mapcar 'vsts/parse-build-item values)))

(bui-define-groups builds
  ;; :parent-group tools
  ;; :parent-faces-group faces
  :group-doc "Settings for '\\[builds]' command."
  :faces-group-doc "Faces for '\\[builds]' command.")

(bui-define-interface builds list
  :buffer-name "*Builds*"
  :titles '((buildNumber . "Number")
	    (sourceBranch . "Source")
	    (sourceVersion . "Version")
	    (result . ""))
  :describe-function #'vsts/builds-list-describe
  :get-entries-function '(lambda ()
			   ;; (message "get builds")
			   (vsts/get-builds))
  :filter-predicates '(builds-build-current-project?
		       builds-build-repository?)
  :format '((result nil 5 t)
	    (name nil 25 t)
	    (buildNumber nil 30 t)
	    (sourceBranch nil 15 t)
	    (sourceVersion nil 10 t)
	    (status nil 10 t)
	    (completed nil 20 t)
	    (requestedBy nil 20 t))
  ;; :sort-key '(name)
  )

(defun builds-build-current-project? (entry)
  "Returns t if builds is in current project"
  t)

(defun builds-build-current-repository? (entry)
  "Returns t if builds is in current repository"
  t)

(bui-define-interface builds info
  :get-entries-function #'builds-info-entries-function
  :format '((buildNumber format (format))
	    builds-info-insert-artifact
	    nil))

(defun builds-info-insert-artifact (entry)
  "Inserts artifact details in builds info panel"
  (when-let
      ((build-id (alist-get 'id entry))
       (artifact (vsts/get-build-artifact build-id))
       (filename (replace-regexp-in-string "drop/" "" (alist-get 'path artifact))))
    (bui-format-insert "Artifact" 'bui-info-param-title bui-info-param-title-format)
    (bui-format-insert filename)
    (bui-insert-indent)
    (bui-insert-action-button "Download"
			      (lambda (f)
				(browse-url (alist-get 'contentLocation (button-get f 'file))))
			      "Download this artifact"
			      'file artifact)))

(defun vsts/get-build-artifact (build)
  "Returns artifacts of `build'"
  (when-let ((base-url (vsts/get-url (format vsts-build-artifacts-api build) t "2.0"))
	     (response (request-response-data (vsts--submit-request base-url nil "GET" nil nil)))
	     (hasData (> (alist-get 'count response) 0))
	     (value (alist-get 'value response))
	     (container (alist-get 'resource (elt value 0)))
	     (container-id (replace-regexp-in-string "#/\\([0-9]*?\\)/drop" "\\1" (alist-get 'data container)))
	     (artifact-response (request-response-data (vsts--submit-request (format "https://%s.visualstudio.com/%s/%s?itemPath=drop" vsts-instance vsts-build-artifact-content-api container-id) nil "GET" nil nil)))
	     (artifact-value (alist-get 'value artifact-response))
	     )
    (elt (remove-if-not '(lambda (x) (string= (alist-get 'itemType x) "file")) artifact-value) 0))) 
(when (symbolp 'evil-emacs-state-modes)
  (add-to-list 'evil-emacs-state-modes 'builds-list-mode)
  (add-to-list 'evil-emacs-state-modes 'builds-info-mode))

(when (boundp 'purpose-user-mode-purposes)
  (add-to-list 'purpose-user-mode-purposes '(builds-info-mode . terminal)))

(defun vsts/queue-build ()
  "Queues new build"
  (interactive)
  (let* ((build-def (ivy-read "select build definition:"
			      '(lambda (&optional input ok we)
				 (vsts/get-build-definition-selection))))
	 (build-def-id (get-text-property 0 'property build-def))
	 (branch (ivy-read "select branch:"
			   '(lambda (&optional input ok we)
			      (vsts/get-git-branches-selection))))
	 (req-params `((definition . ((id . ,build-def-id)))
		       (sourceBranch . ,(substring-no-properties branch)))))
    (vsts--submit-request (vsts/get-url vsts-builds-api t)
			  '(lambda (data)
			     (message "Queued build %s on %s" build-def branch))
			  "POST" req-params)))

(defun builds-list-cancel-build ()
  "Cancels current selected build"
  (interactive)
  (let* ((id (bui-list-current-id))
	 (status (alist-get 'status (bui-list-current-entry))))
    (if (string= status "inProgress")
	(when (yes-or-no-p (message "Cancel build %s?" id))
	  (vsts/cancel-build id))
      (error (format "Cannot cancel build %s because it is not in progress" id)))))

(defun vsts/cancel-build (build-id)
  "Cancels build `build-id'"
  (let ((params '((status . "cancelling"))))
    (vsts--submit-request (vsts/get-url (concat vsts-builds-api "/" (number-to-string build-id)) t)
			  '(lambda (data)
			     (message "Cancelled %s" data))
			  "PATCH"
			  params
			  nil)))

(defun builds-info-entries-function (&rest args)
  "Returns the details for the info entry"
  (list args))

(defun vsts/builds-list-describe (&rest ids)
  "Display 'info' for builds in ids"
  (bui-get-display-entries 'builds 'info (bui-list-current-entry)))

(let ((map builds-list-mode-map))
  (define-key map (kbd "q") 'quit-window)
  (define-key map (kbd "b") 'vsts/queue-build)
  (define-key map (kbd "c") 'builds-list-cancel-build))

(provide 'vsts-builds)
;;; vsts-builds.el ends here

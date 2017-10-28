;;; vsts-workitems.el --- Work Items Api             -*- lexical-binding: t; -*-

;; Copyright (C) 2017 - jtbm37

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
(defconst vsts-workitems-api "_apis/wit/workitems")
(defvar vsts-workitem-fields '("System.Title"
			       "System.State"
			       "System.WorkItemType"
			       "System.AreaPath"
			       "System.TeamProject"
			       "System.IterationPath"
			       "System.AssignedTo"
			       "System.CreatedDate"
			       "System.CreatedBy"
			       "System.ChangedDate"
			       "System.ChangedBy"
			       "System.Description"
			       "Microsoft.VSTS.Common.Priority"
			       "Microsoft.VSTS.Common.Severity"
			       "Microsoft.VSTS.TCM.ReproSteps"
			       "Microsoft.VSTS.TCM.SystemInfo"
			       "Microsoft.VSTS.Common.AcceptanceCriteria"))

(defun vsts/get-work-items (ids &optional fields)
  "Returns work items for the specified ids"
  (when ids
    (let ((url (concat (vsts/get-url vsts-workitems-api)
		       "&ids="
		       (string-join ids ",")
		       (when fields (concat "&fields=" (string-join fields ","))))))
      (alist-get 'value (request-response-data (vsts--submit-request url nil "GET" nil nil))))))

(bui-define-interface vsts-wi info
  :buffer-name "*Work Item*"
  :titles '((System\.State . "State")
	    (System\.Description . "Description")
	    (Microsoft\.VSTS\.Common\.Priority . "Priority")
	    (Microsoft\.VSTS\.Common\.AcceptanceCriteria . "Acceptance Criteria"))
  :get-entries-function '(lambda (&rest args)
			   (let* ((resp (vsts/get-work-items (alist-get 'id args) vsts-workitem-fields))
				  (value (elt resp 0))
				  (fields (alist-get 'fields value)))
			     (list (push (assoc 'id value) fields))))
  :format '(vsts-wi-info-title-insert
	    (System.State format (format))
	    (Microsoft\.VSTS\.Common\.Priority format (format))
	    nil
	    (System.Description vsts-insert-title vsts-insert-html)
	    (Microsoft.VSTS.Common.AcceptanceCriteria vsts-insert-title vsts-insert-html)))

(defun vsts-wi-info-title-insert (entry)
  "Inserts the work item title into info buffer"
  (when-let ((id (alist-get 'id entry))
	     (title (alist-get 'System\.Title entry))
	     (user (alist-get 'System\.AssignedTo entry)))
    (bui-format-insert (format "%s - %s" id title) 'font-lock-builtin-face)
    (bui-newline)
    (bui-format-insert "Assigned To" 'bui-info-param-title bui-info-param-title-format)
    (bui-format-insert (replace-regexp-in-string " <.*?>" "" user) )
    (bui-newline)))

(defun vsts/show-workitem (id)
  "Display the work item details"
  (interactive)
  (bui-get-display-entries 'vsts-wi 'info (list (cons 'id (list id))
						(cons 'url (vsts/get-web-url (format "/_workitems/edit/%s" id))))))

(when (symbolp 'evil-emacs-state-modes)
  ;; (add-to-list 'evil-emacs-state-modes 'vsts-wi-list-mode)
  (add-to-list 'evil-emacs-state-modes 'vsts-wi-info-mode))

(let ((map vsts-wi-info-mode-map))
  (define-key map (kbd "q") 'quit-window)
  (define-key map (kbd "C-o") 'vsts/open-item))

(provide 'vsts-workitems)
;;; vsts-workitems.el ends here

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

;; TODO - To get the html into the buffer, write the html to a temp file and
;; use (shell-command "w3m -dump path/to/temp.html")
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
  :get-entries-function '(lambda (&rest args)
			   (let ((value (vsts/get-work-items (cdr args) vsts-workitem-fields)))
			     (list (elt value 0))))
  :format '((id nil vsts-insert-value)
	    nil))

(defun vsts/show-workitem (id)
  "Display the work item details"
  (interactive)
  (bui-get-display-entries 'vsts-wi 'info (cons 'id (list id))))

(provide 'vsts-workitems)
;;; vsts-workitems.el ends here

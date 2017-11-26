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

(defun vsts/get-work-items (ids &optional fields relation-p)
  "Returns work items for the specified ids

`fields' is a list of fields. Cannot be used when `relation-p' is non-nil.

When `relation-p' is non-nil, it will include the work item relations."
  (when (and ids (> (length (remove-if-not 'nil ids)) 0))
    (let ((url (concat (vsts/get-url vsts-workitems-api)
		       "&ids="
		       (string-join ids ",")
		       (when fields (concat "&fields=" (string-join fields ",")))
		       (when relation-p "&$expand=relations"))))
      (alist-get 'value (request-response-data (vsts--submit-request url nil "GET" nil nil))))))


(defun vsts/get-related-work-items (wi)
  "Returns a list of the related work items
in work item `wi'"
  (when-let ((relations (alist-get 'relations wi))
	     (rel-wis (remove-if-not '(lambda (x) (or (string-equal (alist-get 'rel x) "System.LinkTypes.Hierarchy-Forward")
						 (string-equal (alist-get 'rel x) "System.LinkTypes.Hierarchy-Reverse")))
				     relations))
	     (ids (mapcar 'vsts/get-relation-work-item-id rel-wis))
	     (wis (vsts/get-work-items ids '("System.Title" "System.State"))))
    (mapcar '(lambda (x)
	       (push (assoc 'id x) (alist-get 'fields x)))
	    wis)))

(defun vsts/get-relation-work-item-id (relation)
  "Returns the id of the relation's work item"
  (let* ((url (alist-get 'url relation))
	 (id (-last-item (s-split "/" url))))
    (when (string-to-number id)
      id)))

(defun vsts/create-work-item (type title &optional args parent)
  "Creates work item where `args' is an alist
of the http request params"
  (let ((base-url (vsts/get-url (concat vsts-workitems-api "/$" type) t "1.0"))
	(params `(((op . "add")
		   (path . "/fields/System.Title")
		   (value . ,title)))))
    (when parent
      (push `((op . "add")
	      (path . "/relations/-")
	      (value . ((rel . "System.LinkTypes.Hierarchy-Reverse")
			(url . ,(vsts/get-url (concat vsts-workitems-api "/" parent))))))
	    params))
    (alist-get 'value (request-response-data (vsts--submit-request base-url nil "PATCH" params nil)))))

(defun vsts/create-wi-task (title &optional args parent)
  (vsts/create-work-item "Task" title args parent))

(provide 'vsts-workitems)
;;; vsts-workitems.el ends here

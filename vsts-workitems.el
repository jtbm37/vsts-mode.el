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

(require 'vsts-teams)

(defconst vsts-workitems-api "_apis/wit/workitems")
(defconst vsts-queries-api "_apis/wit/queries")
(defconst vsts-query-workitems-api "_apis/wit/wiql/%s")
(defconst vsts-workitem-comments-api "_apis/wit/workitems/%s/comments")
(defconst vsts-workitem-types-api "_apis/wit/workitemTypes/")

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
						 (string-equal (alist-get 'rel x) "System.LinkTypes.Hierarchy-Reverse")
						 (string-equal (alist-get 'rel x) "System.LinkTypes.Related")))
				     relations))
	     (ids (mapcar 'vsts/get-relation-work-item-id rel-wis))
	     (wis (vsts/get-work-items ids '("System.Title" "System.State" "System\.AssignedTo"))))
    (mapcar '(lambda (x)
	       (push (assoc 'id x) (alist-get 'fields x)))
	    wis)))

(defun vsts/get-relation-work-item-id (relation)
  "Returns the id of the relation's work item"
  (let* ((url (alist-get 'url relation))
	 (id (-last-item (s-split "/" url))))
    (when (string-to-number id)
      id)))

(defun vsts/get-workitem-comments (id)
  "Returns work item comments for `id'"
  (let* ((url (concat (format (vsts/get-url vsts-workitem-comments-api nil "3.0-preview") id) "&fromRevision=1")))
    (request-response-data (vsts--submit-request url nil "GET" nil nil))))

(defun vsts/create-work-item (type title &optional args parent)
  "Creates work item where `args' is an alist
of the http request params"
  (let ((base-url (vsts/get-url (concat vsts-workitems-api "/$" type) t "1.0"))
	(params `(((op . "add")
		   (path . "/fields/System.Title")
		   (value . ,title)))))
    (when args
      (push args params))
    (when parent
      (push `((op . "add")
	      (path . "/relations/-")
	      (value . ((rel . "System.LinkTypes.Hierarchy-Reverse")
			(url . ,(vsts/get-url (concat vsts-workitems-api "/" parent))))))
	    params))
    (alist-get 'value (request-response-data (vsts--submit-request base-url nil "PATCH" params nil)))))

(defun vsts/create-wi-task (title &optional args parent)
  (vsts/create-work-item "Task" title args parent))

(defun vsts/get-work-item-states (type current-state)
  "Returns all possibles states of work item `type'
with `state'"
  (message "getting item state %s %s" type current-state)
  (when-let* ((url (vsts/get-url (concat vsts-workitem-types-api (s-replace " " "%20" type)) t "1.0"))
	      (wi-type (request-response-data (vsts--submit-request url nil "GET")))
	      (wi-trans (alist-get 'transitions wi-type))
	      (possible-trans (alist-get (intern current-state) wi-trans)))
    (sort (mapcar 'cdar possible-trans) 'string<)))

(defun vsts/update-work-item-state (id)
  (interactive "nId: ")
  (when (numberp id)
    (setq id (number-to-string id)))
  (when-let* ((url (concat (vsts/get-url (format "%s/%s" vsts-workitems-api id) nil "1.0")))
	      (wi (elt (vsts/get-work-items (list id)) 0))
	      (rev (alist-get 'rev wi))
	      (fields (alist-get 'fields wi))
	      (type (alist-get 'System.WorkItemType fields))
	      (old-state (alist-get 'System.State fields))
	      (possible-states (vsts/get-work-item-states type old-state))
	      (new-state (ivy-read "Select state: " possible-states))
	      (data `(((op . "test")
		       (path . "/rev")
		       (value . ,rev))
		      ((op . "add")
		       (path . "/fields/System.State")
		       (value . ,new-state)))))
    (when (and new-state (not (string= old-state new-state)))
      (vsts--submit-request url nil "PATCH" data nil))))

(defun vsts/assign-work-item (id)
  "Assigns work item to `name'"
  (interactive "nId: ")
  (when (numberp id)
    (setq id (number-to-string id)))
  (when-let* ((url (concat (vsts/get-url (format "%s/%s" vsts-workitems-api id) nil "1.0")))
	      (wi (elt (vsts/get-work-items (list id)) 0))
	      (rev (alist-get 'rev wi))
	      (fields (alist-get 'fields wi))
	      (old-assignee (or (alist-get 'System.AssignedTo fields) "unassigned"))
	      (team-members (sort (cl-loop for member in (vsts/get-default-teams-members)
					   if (not (alist-get 'isContainer member))
					   collect (propertize (alist-get 'displayName member) 'property member))
				  'string<))
	      (new-assignee (ivy-read "Select state: " team-members))
	      (data `(((op . "test")
		       (path . "/rev")
		       (value . ,rev))
		      ((op . "add")
		       (path . "/fields/System.AssignedTo")
		       (value . ,(alist-get 'uniqueName (get-text-property 0 'property new-assignee)))))))
    (when (and new-assignee (or (not (s-contains? new-assignee old-assignee))
				(user-error "Work item %s is already assigned to %s" id old-assignee)))
      (if (= (request-response-status-code (vsts--submit-request url nil "PATCH" data nil)) 200)
	  (message "Assigned wi %s to %s from %s" id (alist-get 'uniqueName (get-text-property 0 'property new-assignee)) old-assignee)
	(user-error "Could not assign work item %s from %s to %s" id new-assignee old-assignee)))))

(defun vsts/unassign-work-item (id)
  "Assigns work item to `name'"
  (interactive "nId: ")
  (when (numberp id)
    (setq id (number-to-string id)))
  (when-let* ((url (concat (vsts/get-url (format "%s/%s" vsts-workitems-api id) nil "1.0")))
	      (wi (elt (vsts/get-work-items (list id)) 0))
	      (rev (alist-get 'rev wi))
	      (fields (alist-get 'fields wi))
	      (old-assignee (alist-get 'System.AssignedTo fields))
	      (data `(((op . "test")
		       (path . "/rev")
		       (value . ,rev))
		      ((op . "add")
		       (path . "/fields/System.AssignedTo")
		       (value . "")))))
    (if (= (request-response-status-code (vsts--submit-request url nil "PATCH" data nil)) 200)
	(message "Unassigned wi %s from %s" id old-assignee)
      (user-error "Could not assign work item %s" id))))


(provide 'vsts-workitems)
;;; vsts-workitems.el ends here

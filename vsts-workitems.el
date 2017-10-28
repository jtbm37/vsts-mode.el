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

(bui-define-interface vsts-wi info
  :buffer-name "*Work Item*"
  :titles '((System\.State . "State")
	    (System\.Description . "Description")
	    (Microsoft\.VSTS\.Common\.Priority . "Priority")
	    (Microsoft\.VSTS\.Common\.AcceptanceCriteria . "Acceptance Criteria"))
  :get-entries-function '(lambda (&rest args)
			   (list args))
  :format '(vsts-wi-info-title-insert
	    (System.State format (format))
	    (Microsoft\.VSTS\.Common\.Priority format (format))
	    nil
	    (System.Description vsts-insert-title vsts-insert-html)
	    (Microsoft.VSTS.Common.AcceptanceCriteria vsts-insert-title vsts-insert-html)
	    vsts-wi-info-relations-insert))

(defun vsts/get-work-item-info (id)
  "Returns work details necessary for the info panel"
  (when (wholenump id)
    (setq id (number-to-string id)))
  (when-let ((resp (vsts/get-work-items (list id) nil t))
	     (value (elt resp 0))
	     (fields (alist-get 'fields value))
	     (wi-id (assoc 'id value))
	     (relations (assoc 'relations value)))
    (push wi-id fields)
    (push relations fields)
    fields))

(defun vsts-wi-info-title-insert (entry)
  "Inserts the work item title into info buffer"
  (when-let ((id (alist-get 'id entry))
	     (title (alist-get 'System\.Title entry))
	     (user (or (alist-get 'System\.AssignedTo entry) "N/A")))
    (bui-format-insert (format "%s - %s" id title) 'font-lock-builtin-face)
    (bui-newline)
    (bui-format-insert "Assigned To" 'bui-info-param-title bui-info-param-title-format)
    (bui-format-insert (replace-regexp-in-string " <.*?>" "" user) )
    (bui-newline)))

(defun vsts-wi-info-relations-insert (entry)
  "Inserts the related items into info buffer"
  (when-let ((wis (vsts/get-related-work-items entry)))
    (bui-format-insert "Related Work Items" 'bui-info-param-title nil)
    (bui-newline)
    (seq-doseq (rel wis)
      (bui-format-insert (format "%s - %s (%s)" (alist-get 'id rel) (alist-get 'System\.Title rel) (alist-get 'System\.State rel)))
      (bui-newline))))

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

(defun vsts/show-workitem (id)
  "Display the work item details"
  (interactive "nId:")
  (let ((wi (vsts/get-work-item-info id)))
    (push (cons 'url (vsts/get-web-url (format "/_workitems/edit/%s" id))) wi)
    (bui-get-display-entries 'vsts-wi 'info wi)))

(defun vsts/open-related-wi ()
  "Prompts related work items and then opens it."
  (interactive)
  (when-let ((current-wi (bui-current-args))
	     (related (vsts/get-related-work-items current-wi)))
    (if (= (length related) 1)
	(vsts/show-workitem (number-to-string (alist-get 'id (car related))))
      (ivy-read "select work item:"
		(mapcar '(lambda (x) (propertize (format "%s - %s" (alist-get 'id x) (alist-get 'System.Title x)) 'property (alist-get 'id x))) related)
		:action '(lambda (x) (vsts/show-workitem (number-to-string (get-text-property 0 'property x))))))))

(when (symbolp 'evil-emacs-state-modes)
  ;; (add-to-list 'evil-emacs-state-modes 'vsts-wi-list-mode)
  (add-to-list 'evil-emacs-state-modes 'vsts-wi-info-mode))

(let ((map vsts-wi-info-mode-map))
  (define-key map (kbd "q") 'quit-window)
  (define-key map (kbd "C-o") 'vsts/open-item)
  (define-key map (kbd "gr") 'vsts/open-related-wi))

(provide 'vsts-workitems)
;;; vsts-workitems.el ends here

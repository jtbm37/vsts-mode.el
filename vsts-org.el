;;; vsts-org-mode.el --- Work Org mode             -*- lexical-binding: t; -*-

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

;; Minor mode for org mode

;;; Code:
;;* Requires
(require 'org)

(defgroup org-vsts nil
  "Customisation group for org-vsts."
  :tag "Org Vsts"
  :group 'org)

(defvar org-vsts-mode-hook nil)

;;* Vars
(defvar org-vsts-mode-map
  (let ((org-vsts-map (make-sparse-keymap)))
    (define-key org-vsts-map (kbd "C-c vr") 'org-vsts-visit-related)
    (define-key org-vsts-map (kbd "C-c vb") 'org-vsts-browse)
    (define-key org-vsts-map (kbd "C-c vB") 'org-vsts-browse-related)
    (define-key org-vsts-map (kbd "C-c vc") 'org-vsts-show-pr-changes)
    (define-key org-vsts-map (kbd "C-c vs") 'org-vsts-change-state)
    (define-key org-vsts-map (kbd "C-c va") 'org-vsts-assign)
    org-vsts-map))

(defvar org-vsts-previous-item nil
  "Holds previously visited work item.
This is set when visiting a related item
via `org-vsts-visit-related'")

;;* Mode

;;;###autoload
(define-minor-mode org-vsts-mode
  "Toggle org-vsts mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.
"
  :init-value nil
  :lighter " vsts"
  :group 'org-vsts
  :keymap org-vsts-mode-map

  (if org-vsts-mode
      (run-mode-hooks 'org-vsts-mode-hook)))
;;* Navigation
(defun org-vsts-browse ()
  (interactive)
  (when-let ((url (alist-get 'url vsts/current-wi)))
    (browse-url url)))

(defun org-vsts-change-state ()
  (interactive)
  (when-let ((id (alist-get 'id vsts/current-wi)))
    (vsts/update-work-item-state (number-to-string id))))

(defun org-vsts-assign ()
  (interactive)
  (when-let ((id (alist-get 'id vsts/current-wi)))
    (vsts/assign-work-item (number-to-string id))))

(defun org-vsts-visit-previous-item ()
  (interactive)
  (when-let ((id (alist-get 'id org-vsts-previous-item)))
    (vsts/show-workitem (number-to-string id))))

(defun org-vsts-browse-related ()
  (interactive)
  (when-let ((related (alist-get 'relations vsts/current-wi)))
    (if (= (length related) 1)
	(browse-url (vsts/get-web-url (format "/_workitems/edit/%s" (number-to-string (alist-get 'id (elt related 0))))))
      (ivy-read "select work item:"
		(mapcar '(lambda (x) (propertize (format "%s - %s" (alist-get 'id x) (or (alist-get 'System.Title x) (alist-get 'System.Title (alist-get 'fields x)))) 'property (alist-get 'id x))) related)
		:action '(lambda (x) (browse-url (vsts/get-web-url (format "/_workitems/edit/%s" (number-to-string (get-text-property 0 'property x))))))))))

(defun org-vsts-visit-related ()
  (interactive)
  (when-let ((related (alist-get 'relations vsts/current-wi)))
    (if (= (length related) 1)
	(vsts/show-workitem (number-to-string (alist-get 'id (elt related 0))))
      (ivy-read "select work item:"
		(mapcar '(lambda (x) (propertize (format "%s - %s" (alist-get 'id x) (or (alist-get 'System.Title x) (alist-get 'System.Title (alist-get 'fields x)))) 'property (alist-get 'id x))) related)
		:action '(lambda (x) (vsts/show-workitem (number-to-string (get-text-property 0 'property x))))))))

(defun org-vsts-show-pr-changes ()
  (interactive)
  (when-let ((source (alist-get 'sourceBranch vsts/current-wi))
	     (dest (alist-get 'destBranch vsts/current-wi)))
    (let ((default-directory vsts-project-path)
	  (branch (format "origin/%s" source)))
      (if (string= (magit-get-current-branch) dest)
	  (magit-merge-preview branch)
	(user-error (format "Current branch is not %s" dest))))))

;;* Buffer creation
(defun vsts/create-wi-org-buffer (wi)
  "Creates a temp org-mode buffer
with all `wi' details"
  (when-let ((buffer (get-buffer "*VSTS WORK ITEM*"))
	     (curr-wi (buffer-local-value 'vsts/current-wi buffer)))
    (setq org-vsts-previous-item curr-wi))
  (with-current-buffer-window "*VSTS WORK ITEM*"
			      nil
			      nil
			      (let ((related-wis (alist-get 'relations wi))
				    (comments (alist-get 'comments wi))
				    (pullrequests (alist-get 'pullrequests wi)))
				(org-mode)
				(org-vsts-mode)
				(setq-local vsts/current-wi wi)
				(save-excursion (insert (format "* %s " (vsts/get-org-state (alist-get 'System.State wi))))
						(insert (alist-get 'System.Title wi))
						(org-return)
						(insert "\n\n")
						;; Microsoft.VSTS.TCM.SystemInfo
						(when-let ((sys-info (alist-get 'Microsoft.VSTS.TCM.SystemInfo wi)))
						  (insert "** System Info")
						  (org-return-indent)
						  (vsts-insert-html sys-info nil)
						  (org-return))
						(when-let ((desc (alist-get 'System.Description wi)))
						  (insert "** Description")
						  (org-return-indent)
						  (vsts-insert-html desc nil)
						  (org-return))
						(insert "** Acceptance Criteria")
						(org-return-indent)
						(when-let ((ac (alist-get 'Microsoft.VSTS.Common.AcceptanceCriteria wi)))
						  (vsts-insert-html ac nil))
						(org-return)
						(when related-wis
						  (insert "** Related")
						  (org-return)
						  (seq-doseq (rel related-wis)
						    (when-let ((rel-title (alist-get 'System.Title rel))
							       (rel-state (alist-get 'System.State rel))
							       (rel-id (alist-get 'id rel)))
						      (save-excursion (insert (format "*** %s " (vsts/get-org-state rel-state)))
								      (insert rel-title)
								      (org-return))
						      (org-entry-put (point) "id" (int-to-string rel-id))
						      (org-entry-put (point) "state" rel-state)
						      (when-let ((assigned (alist-get 'System.AssignedTo rel)))
							(org-entry-put (point) "assigned" (replace-regexp-in-string " <.*?>" "" assigned))))))
						(end-of-buffer)
						(org-return)
						(org-return)
						(when (and pullrequests (> (length pullrequests) 0))
						  (insert "** Pull Requests")
						  (org-return)
						  (seq-doseq (pr pullrequests)
						    (when-let ((pr-title (alist-get 'title pr))
							       (pr-status (alist-get 'status pr))
							       (pr-id (alist-get 'pullRequestId pr))
							       (pr-createdBy (alist-get 'displayName (alist-get 'createdBy pr)))
							       (pr-createdAt (alist-get 'creationDate pr)))
						      (save-excursion (insert (format "*** %s PR - " (cond ((string-equal pr-status "completed")
													    "DONE")
													   ((string-equal pr-status "abandoned")
													    "CANCELLED")
													   (t "TODO"))))
								      (insert pr-title)
								      (org-return))
						      (org-entry-put (point) "type" "Pull Request")
						      (org-entry-put (point) "id" (int-to-string pr-id))
						      (org-entry-put (point) "status" pr-status)
						      (org-entry-put (point) "createdBy" pr-createdBy)
						      (org-entry-put (point) "created" pr-createdAt)
						      (when-let ((pr-closedDate (alist-get 'closedDate pr)))
							(org-entry-put (point) "closed" pr-closedDate)))))
						(end-of-buffer)
						(org-return)
						(org-return)
						(when (and comments (> (length comments) 0))
						  (insert "** Comments")
						  (org-return)
						  (seq-doseq (cmt comments)
						    (when-let* ((text (alist-get 'text cmt))
								(author (alist-get 'revisedBy cmt))
								(name (alist-get 'displayName author))
								(date (alist-get 'revisedDate cmt)))
						      (insert (format "*** %s on %s: " name date))
						      (vsts-insert-html text nil)))))
				(org-entry-put (point) "type" (alist-get 'System.WorkItemType wi))
				(org-entry-put (point) "id" (int-to-string (alist-get 'id wi)))
				(org-entry-put (point) "state" (alist-get 'System.State wi))
				(when-let ((assigned (alist-get 'System.AssignedTo wi)))
				  (org-entry-put (point) "assigned" (replace-regexp-in-string " <.*?>" "" assigned)))
				(org-entry-put (point) "iteration" (alist-get 'System.IterationPath wi))
				(org-entry-put (point) "area" (alist-get 'System.AreaPath wi))
				(org-entry-put (point) "created" (alist-get 'System.CreatedDate wi))
				(outline-show-all))))

(defun vsts/create-pr-org-buffer (pr)
  "Creates a temp org-mode buffer
with all `pr' details"
  (with-current-buffer-window "*VSTS PULL REQUEST*"
			      nil
			      nil
			      (progn
				(org-mode)
				(org-vsts-mode)
				(setq-local vsts/current-wi pr)
				(save-excursion (insert (format "* %s" (alist-get 'title pr)))
						(org-return)
						(insert "\n\n")
						(insert "** Related")
						(org-return)
						(when-let ((rels (alist-get 'relations pr)))
						  (seq-doseq (rel rels)
						    (let* ((fields (alist-get 'fields rel))
							   (state (alist-get 'System.State fields)))
						      (save-excursion (insert (format "*** %s " (vsts/get-org-state state)))
								      (insert (alist-get 'System.Title fields))
								      (org-return))
						      (org-entry-put (point) "id" (int-to-string (alist-get 'id rel)))
						      (org-entry-put (point) "state" state)
						      (when-let ((assigned (alist-get 'System.AssignedTo fields)))
							(org-entry-put (point) "assigned" (replace-regexp-in-string " <.*?>" "" assigned))))))
						(end-of-buffer)
						(org-return)
						(org-return)
						(insert "** Comments")
						(org-return)
						(when-let ((threads (alist-get 'comments pr)))
						  (seq-doseq (thread threads)
						    (when-let* ((comments (alist-get 'comments thread))
								(hasUserTypeCmts (cl-some (lambda (x) (not (string= (alist-get 'commentType x) "system"))) comments))
								(firstComment (seq-elt comments 0)))
						      (if-let* ((thread-context (alist-get 'threadContext thread))
						      	       (file-path (alist-get 'filePath thread-context)))
						      	  (insert (format "*** %s: " file-path))
						      	(let ((name (alist-get 'displayName (alist-get 'author firstComment))))
							  (insert (format "*** %s said: " name))))
						      (org-return)
						      (seq-doseq (cmt comments)
						      	(let ((content (alist-get 'content cmt))
						      	      (name (alist-get 'displayName (alist-get 'author cmt)))
						      	      (date (alist-get 'lastContentUpdatedDate cmt)))
						      	  (insert (format "**** %s on %s" name date))
							  (org-return)
						      	  (vsts-insert-html content nil)
							  (org-return)))))))
				;; (org-entry-put (point) "type" (alist-get 'System.WorkItemType pr))
				(org-entry-put (point) "type" "Pull Request")
				(org-entry-put (point) "id" (int-to-string (alist-get 'id pr)))
				(org-entry-put (point) "status" (alist-get 'status pr))
				(org-entry-put (point) "createBy" (alist-get 'createdBy pr))
				;; (when-let ((assigned (alist-get 'System.AssignedTo pr)))
				;;   (org-entry-put (point) "assigned" (replace-regexp-in-string " <.*?>" "" assigned)))
				(org-entry-put (point) "createdAgo" (alist-get 'createdAgo pr))
				;; (org-entry-put (point) "area" (alist-get 'System.AreaPath pr))
				(outline-show-all))))

;;* Misc
(defun vsts/get-org-state (wi-state)
  "Returns the corresponding org state
for `wi-state'"
  (cond ((string-equal "Done" wi-state)
	 "DONE")
	(t
	 "TODO")))

;;* Provide
(provide 'vsts-org)

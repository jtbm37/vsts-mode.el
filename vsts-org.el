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
(require 'org)

(defgroup org-vsts nil
  "Customisation group for org-vsts."
  :tag "Org Vsts"
  :group 'org)

(defvar org-vsts-mode-hook nil)

(defvar org-vsts-mode-map
  (let ((org-vsts-map (make-sparse-keymap)))
    (define-key org-vsts-map (kbd "C-c vr") 'org-vsts-visit-related)
    (define-key org-vsts-map (kbd "C-c vb") 'org-vsts-browse)
    (define-key org-vsts-map (kbd "C-c vB") 'org-vsts-browse-related)
    org-vsts-map))

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

(defun org-vsts-browse ()
  (interactive)
  (when-let ((url (alist-get 'url vsts/current-wi)))
    (browse-url url)))

(defun org-vsts-browse-related ()
  (interactive)
  (when-let ((related (alist-get 'relations vsts/current-wi)))
    (if (= (length related) 1)
	(browse-url (vsts/get-web-url (format "/_workitems/edit/%s" (number-to-string (alist-get 'id (elt related 0))))))
      (ivy-read "select work item:"
		(mapcar '(lambda (x) (propertize (format "%s - %s" (alist-get 'id x) (alist-get 'System.Title x)) 'property (alist-get 'id x))) related)
		:action '(lambda (x) (browse-url (vsts/get-web-url (format "/_workitems/edit/%s" (number-to-string (get-text-property 0 'property x))))))))))

(defun org-vsts-visit-related ()
  (interactive)
  (when-let ((related (alist-get 'relations vsts/current-wi)))
    (if (= (length related) 1)
	(vsts/show-workitem (number-to-string (alist-get 'id (elt related 0))))
      (ivy-read "select work item:"
		(mapcar '(lambda (x) (propertize (format "%s - %s" (alist-get 'id x) (alist-get 'System.Title x)) 'property (alist-get 'id x))) related)
		:action '(lambda (x) (vsts/show-workitem (number-to-string (get-text-property 0 'property x))))))))

(defun vsts/create-org-buffer (wi)
  "Creates a temp org-mode buffer
with all `wi' details"
  (with-current-buffer-window "*VSTS WORK ITEM*"
			      nil
			      nil
			      (progn
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
						(insert "** Related")
						(org-return)
						(when-let ((rels (alist-get 'relations wi)))
						  (seq-doseq (rel rels)
						    (save-excursion (insert (format "*** %s " (vsts/get-org-state (alist-get 'System.State rel))))
								    (insert (alist-get 'System.Title rel))
								    (org-return))
						    (org-entry-put (point) "id" (int-to-string (alist-get 'id rel)))
						    (org-entry-put (point) "state" (alist-get 'System.State rel))
						    (when-let ((assigned (alist-get 'System.AssignedTo rel)))
						      (org-entry-put (point) "assigned" (replace-regexp-in-string " <.*?>" "" assigned))))))
				(org-entry-put (point) "type" (alist-get 'System.WorkItemType wi))
				(org-entry-put (point) "id" (int-to-string (alist-get 'id wi)))
				(org-entry-put (point) "state" (alist-get 'System.State wi))
				(when-let ((assigned (alist-get 'System.AssignedTo wi)))
				  (org-entry-put (point) "assigned" (replace-regexp-in-string " <.*?>" "" assigned)))
				(org-entry-put (point) "iteration" (alist-get 'System.IterationPath wi))
				(org-entry-put (point) "area" (alist-get 'System.AreaPath wi))
				(outline-show-all))))

(defun vsts/get-org-state (wi-state)
  "Returns the corresponding org state
for `wi-state'"
  (cond ((string-equal "Done" wi-state)
	 "DONE")
	(t
	 "TODO")))

(provide 'vsts-org)

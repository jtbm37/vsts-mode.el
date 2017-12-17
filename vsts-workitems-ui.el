;;; vsts-workitems-ui.el --- Work Items UI             -*- lexical-binding: t; -*-

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
(require 'vsts-workitems)
(require 'vsts-org)

(defvar vsts-wi-queries nil)

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
	     (wi-id (assoc 'id value)))
    (push wi-id fields)
    (when (assoc 'relations value)
      (push (assoc 'relations value) fields))
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
  (when-let ((wis (alist-get 'relations entry)))
    (bui-format-insert "Related Work Items" 'bui-info-param-title nil)
    (bui-newline)
    (seq-doseq (rel wis)
      (bui-format-insert (format "%s - %s (%s)" (alist-get 'id rel) (alist-get 'System\.Title rel) (alist-get 'System\.State rel)))
      (bui-newline))))

(defun vsts/get-queries ()
  (unless vsts-wi-queries
    (when-let* ((url (concat (vsts/get-url (concat vsts-queries-api "/My%20Queries") t "1.0")
			     "&$depth=1"))
		(data (request-response-data (vsts--submit-request url nil "GET" nil nil)))
		(children (alist-get 'children data)))
      (setq vsts-wi-queries children)))
  vsts-wi-queries)

;;;###autoload
(defun vsts/show-query ()
  (interactive)
  (vsts/set-credentials)
  (ivy-read "select query:"
	    (mapcar '(lambda (x) (propertize (format "%s" (alist-get 'name x)) 'property (alist-get 'id x))) (vsts/get-queries))
	    :action '(lambda (x) (vsts/show-query-workitems (get-text-property 0 'property x)))))

;;;###autoload
(defun vsts/show-workitem (id)
  "Display the work item details"
  (interactive "sId:")
  (let ((wi (vsts/get-work-item-for-display id)))
    (vsts/create-org-buffer wi)))

(defun vsts/get-work-item-for-display (id)
  (let* ((wi (vsts/get-work-item-info id))
	 (related (vsts/get-related-work-items wi))
	 (comments (assoc 'comments (vsts/get-workitem-comments id))))
    (push (cons 'url (vsts/get-web-url (format "/_workitems/edit/%s" id))) wi)
    (push comments wi)
    (when (and related (> (length related) 0))
      (assq-delete-all 'relations wi)
      (push (cons 'relations related) wi))
    wi))

(bui-define-groups workitems
  :group-doc "Settings for '\\[queries]' command."
  :faces-group-doc "Faces for '\\[queries]' command.")

(bui-define-interface workitems list
  :buffer-name "*Work Items*"
  :titles '((System\.Id . "Id")
	    (System\.Title . "Title")
	    (System\.State . "State")
	    (Microsoft\.VSTS\.Common\.Priority . "Priority")
	    (System\.ChangedDate . "Changed")
	    (System\.IterationPath . "Iteration")
	    (System\.AreaPath . "Area"))
  :describe-function #'vsts/workitems-list-describe
  :get-entries-function '(lambda (&rest id)
			   (vsts/get-query-workitems id))
  ;; :filter-predicates '(builds-build-current-project?
  ;; 		       builds-build-repository?)
  :format '((System.Id nil 5 t)
	    (System.Title nil 100 t)
	    (System.State nil 10 t)
	    (Microsoft.VSTS.Common.Priority nil 5 t)
	    (System.ChangedDate nil 25 t)
	    (System.IterationPath nil 20 t)
	    (System.AreaPath nil 25 t)))

(defun vsts/show-query-workitems (id)
  ""
  (bui-get-display-entries 'workitems 'list (list id)))

(defun vsts/get-query-workitems (id)
  (when-let* ((base-url (format (vsts/get-url vsts-query-workitems-api t "1.0") id))
	      (query-data (request-response-data (vsts--submit-request base-url nil "GET" nil nil)))
	      (wis (mapcar (lambda (x) (number-to-string (alist-get 'id x))) (alist-get 'workItems query-data))))
    (when (> (length wis) 50)
      (message "Query has %s items. Showing only 50." (length wis))
      (setq wis (-take 50 wis)))
    ;; `vsts/get-work-items' does not return the items in the same order
    ;; as `wis'. So we have to iterate through wis and get the item from `wis-full'
    (let ((wis-full (vsts/get-work-items wis '("System\.Id" "System\.Title" "System\.State" "Microsoft\.VSTS\.Common\.Priority" "System\.ChangedDate" "System\.IterationPath" "System\.AreaPath"))))
      (mapcar (lambda (x)
		(cons `(id . ,x) (alist-get 'fields (seq-find (lambda (w) (eq (string-to-number x) (alist-get 'id w))) wis-full))))
	      wis))))


(defun vsts/workitems-list-describe (&rest ids)
  (vsts/show-workitem (number-to-string (alist-get 'System\.Id (bui-list-current-entry)))))

(defun vsts/open-related-wi ()
  "Prompts related work items and then opens it."
  (interactive)
  (when-let ((current-wi (bui-current-args))
	     (related (alist-get 'relations current-wi)))
    (if (= (length related) 1)
	(vsts/show-workitem (number-to-string (alist-get 'id (elt related 0))))
      (ivy-read "select work item:"
		(mapcar '(lambda (x) (propertize (format "%s - %s" (alist-get 'id x) (alist-get 'System.Title x)) 'property (alist-get 'id x))) related)
		:action '(lambda (x) (vsts/show-workitem (number-to-string (get-text-property 0 'property x))))))))

(when (symbolp 'evil-emacs-state-modes)
  (add-to-list 'evil-emacs-state-modes 'vsts-wi-info-mode)
  (add-to-list 'evil-emacs-state-modes 'workitems-list-mode))

(let ((map vsts-wi-info-mode-map))
  (define-key map (kbd "q") 'quit-window)
  (define-key map (kbd "C-o") 'vsts/open-item)
  (define-key map (kbd "gr") 'vsts/open-related-wi))

(provide 'vsts-workitems-ui)
;;; vsts-workitems.el ends here

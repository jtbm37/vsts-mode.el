;;; vsts-git.el --- VSTS GIT API                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  jtbm37

;; Author: jtbm37
;; Keywords: tools

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

(defconst vsts-git-branch-api "_apis/git/repositories/%s/refs/heads")
(defconst vsts-git-pullrequests-api "_apis/git/repositories/%s/pullrequests")
(defconst vsts-git-repository-api "_apis/git/repositories/")

(defvar vsts-git-repository nil
  "Holds the repository metadata for current `vsts-repository'")

(defvar vsts-git-branches nil
  "Holds all branches for repository `vsts-project'")

(defun vsts/git-get-repository ()
  "Gets the current repository from remote if nil, otherwise return `vsts-git-repository'"
  (unless vsts-git-repository
    (let ((base-url (vsts/get-url (concat vsts-git-repository-api vsts-repository) t "1.0")))
      (setq vsts-git-repository (request-response-data (vsts--submit-request base-url nil "GET" nil nil)))))
  vsts-git-repository)

(defun vsts/git-get-branches ()
  "Returns all git branches for the current repository in `vsts-repository'"
  (unless vsts-git-branches
    (let ((base-url (vsts/get-url (format vsts-git-branch-api (alist-get 'id  (vsts/git-get-repository))) t "1.0")))
      (vsts--submit-request base-url '(lambda (data) (setq vsts-git-branches data)) "GET" nil nil)))
  vsts-git-branches)

(defun vsts/get-git-branches-selection ()
  "Returns list of branches to feed a user selection"
  (mapcar '(lambda (x) (propertize (alist-get 'name x) 'property (alist-get 'objectId x))) (cdr (car (vsts/git-get-branches)))))

(defun vsts/git-get-pullrequests (&optional id)
  "Returns all pull requests in `vsts-repository'"
  (let ((base-url (vsts/get-url (concat (format vsts-git-pullrequests-api (alist-get 'id  (vsts/git-get-repository))) (when id (format "/%s" id))) t "3.0-preview")))
    (request-response-data (vsts--submit-request base-url nil "GET" nil nil))))

(defun vsts/git-parse-pullrequest (pr)
  "Parses a pull request and returns relevant properties for the list"
  (let ((pr-id (alist-get 'pullRequestId pr))
	result)
    (push (cons 'id pr-id) result)
    (push (cons 'createdBy (alist-get 'displayName (alist-get 'createdBy pr))) result)
    (push (assoc 'status pr) result)
    (push (assoc 'description pr) result)
    (push (cons 'createdAgo (time-to-ago (alist-get 'creationDate pr))) result)
    (push (assoc 'title pr) result)
    (push (cons 'url (concat (vsts/get-web-url (format "/_git/%s/pullrequest/%s" vsts-repository pr-id)))) result)
    (push (cons 'sourceBranch (replace-regexp-in-string "refs/heads/" "" (alist-get 'sourceRefName pr))) result)
    (push (cons 'destBranch (replace-regexp-in-string "refs/heads/" "" (alist-get 'targetRefName pr))) result)
    result))

(bui-define-interface vsts-pullrequests list
  :buffer-name "*Pull Requests*"
  ;; :titles '(
  ;; 	    )
  :describe-function #'vsts-pullrequests-list-describe
  :get-entries-function '(lambda ()
			   (mapcar 'vsts/git-parse-pullrequest (alist-get 'value (vsts/git-get-pullrequests))))
  :format '((id nil 5 t)
	     (createdBy nil 15 t)
	     (title nil 80 t)
	     (createdAgo nil 15 t)
	     (sourceBranch nil 20 t)
	     (destBranch nil 20 t)))

(defun vsts-pullrequests-list-describe (&rest pr)
  "Display 'info' buffer for vsts-pullrequests list."
  (bui-get-display-entries 'vsts-pullrequests 'info (cons 'id pr)))

(bui-define-interface vsts-pullrequests info
  :buffer-name "*Pull Request Info*"
  :get-entries-function '(lambda (&rest args)
			   (let ((pr (cdr (vsts/git-get-pullrequests (cadr args)))))
			     (list (vsts/git-parse-pullrequest pr))))
  :format '((title nil vsts-insert-value)
	    (description vsts-insert-title vsts-insert-value)
	    vsts-pr-info-work-items-insert
	    nil))

(defun vsts-insert-title (args)
  "Inserts title"
  (bui-format-insert args 'bui-info-param-title nil)
  (bui-newline))

(defun vsts-insert-value (value args)
  "Inserts value. `args' is the full alist entry"
  (bui-split-insert value nil 70)
  (bui-newline))

(defun vsts-insert-html (html args)
  "Converts `html' as text and inserts value.
`args' is the full alist entry"
  (with-temp-file "/tmp/emacs-vsts-temp.html"
    (erase-buffer)
    (insert html))
  (call-process  "/usr/local/bin/w3m" nil t t "/tmp/emacs-vsts-temp.html")
  (bui-newline))

(defun vsts-pr-info-work-items-insert (entry)
  "inserts work items section in pull request info panel"
  (when-let ((pr-id (alist-get 'id entry))
	     (wis (vsts/get-pullrequest-work-items pr-id)))
    (bui-format-insert "Work Items" 'bui-info-param-title bui-info-param-title-format)
    (bui-newline)
    (seq-doseq (wi wis)
      (bui-format-insert (format "#%s - %s" (alist-get 'id wi) (alist-get 'System\.Title (alist-get 'fields wi))))
      (bui-newline))))

(defun vsts/get-pullrequest-work-items (pr-id)
  "Returns pull request's work items for `pr-id'"
  (when-let ((url (vsts/get-url (format "%s/%s/workitems" (format vsts-git-pullrequests-api (alist-get 'id  (vsts/git-get-repository))) pr-id)))
	     (resp (request-response-data (vsts--submit-request url nil "GET" nil nil)))
	     (hasData (> (alist-get 'count resp) 0))
	     (wis-id (mapcar '(lambda (x) (alist-get 'id x)) (alist-get 'value resp))))
    (vsts/get-work-items wis-id '("System.Title"))))

(let ((map vsts-pullrequests-list-mode-map))
  (define-key map (kbd "q") 'quit-window)
  (define-key map (kbd "C-o") 'vsts/open-item))

(when (symbolp 'evil-emacs-state-modes)
  (add-to-list 'evil-emacs-state-modes 'vsts-pullrequests-list-mode)
  (add-to-list 'evil-emacs-state-modes 'vsts-pullrequests-info-mode))

;;;###autoload
(defun vsts/show-pullrequests ()
  "Shows list of pull requests in `vsts-repository'"
  (interactive)
  (vsts/set-credentials)
  (bui-get-display-entries 'vsts-pullrequests 'list))

(provide 'vsts-git)
;;; vsts-git.el ends here

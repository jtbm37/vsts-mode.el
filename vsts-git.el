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
(defconst vsts-git-repository-api "_apis/git/repositories/")

(defvar vsts-git-repository nil
  "Holds the repository metadata for current `vsts-repository'")

(defvar vsts-git-branches nil
  "Holds all branches for repository `vsts-project'")

(defun vsts/git-get-repository ()
  "Gets the current repository from remote if nil, otherwise return `vsts-git-repository'"
  (unless vsts-git-repository
    (let ((base-url (vsts/get-url (concat vsts-git-repository-api vsts-repository) t "1.0")))
      (message "Querying %s" base-url)
      (vsts--submit-request base-url '(lambda (data) (setq vsts-git-repository data)) "GET" nil nil)))
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

(provide 'vsts-git)
;;; vsts-git.el ends here

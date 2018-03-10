;;; vsts-teams.el --- Teams Api             -*- lexical-binding: t; -*-

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

(defvar vsts-teams-api "_apis/projects/%s/teams")

(defvar vsts-default-teams-members-cache nil
  "Holds cache of all `vsts-default-teams' members.
This is populated by `vsts/get-default-teams-members'to avoid
hitting the api all the team.")

(defcustom vsts-default-teams nil
  "Sets the default teams to select members,
when prompting to select a user."
  :type 'string
  :group 'vsts)

(defun vsts--get-teams-url ()
  (format vsts-teams-api vsts-project))

(defun vsts/get-teams ()
  "Returns all teams of `vsts-project'"
  (let ((url (vsts/get-url (vsts--get-teams-url) nil "1.0")))
    (cdar (request-response-data (vsts--submit-request url nil "GET")))))

(defun vsts/get-team-member (team)
  (let ((url (vsts/get-url (format "%s/%s/members" (vsts--get-teams-url) (s-replace " " "%20" team)) nil "1.0")))
    (cdar (request-response-data (vsts--submit-request url nil "GET")))))

(defun vsts/get-default-teams-members ()
  "Returns all members of `vsts-default-teams'"
  ;; TODO prompt a team to select members from when `vsts-default-teams' is nil
  (if vsts-default-teams
      ;; Refresh cache if passed with prefix argument
      (or (and (not (equal current-prefix-arg '(4))) vsts-default-teams-members-cache)
	  (let (all-members
		team-members)
	    (dolist (team vsts-default-teams)
	      (setq team-members (vsts/get-team-member team))
	      (seq-doseq (mbr team-members)
		(push mbr all-members)))
	    (setq vsts-default-teams-members-cache all-members)))
    (user-error "Please set `vsts-default-teams'")))

(provide 'vsts-teams)

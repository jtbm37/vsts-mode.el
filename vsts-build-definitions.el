;;; vsts-build-definitions.el --- Build Definitions Api  -*- lexical-binding: t; -*-

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

(defvar vsts-build-definitions nil
  "Holds all build definitions")

(defun vsts/get-build-definitions ()
  "Returns all build definitions"
  (unless vsts-build-definitions
    (vsts--submit-request (vsts/get-url vsts-build-definitions-api t) (lambda (data) (setq vsts-build-definitions data)) "GET" nil nil))
  vsts-build-definitions)

(defun vsts/get-build-definition-selection ()
  "Returns a list of build defs from `vsts-build-definitions' to feed a user selection."
  (mapcar '(lambda (x) (propertize (alist-get 'name x) 'property (alist-get 'id x))) (cdr (cadr (vsts/get-build-definitions)))))

(provide 'vsts-build-definitions)
;;; vsts-build-definitions.el ends here

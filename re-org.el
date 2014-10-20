;;; re-org.el --- Control ReOrg from Emacs.

;; Copyright (C) 2014 Carlos Ivan Sosa <gnusosa AT gnusosa.net>

;; Author: Carlos Ivan Sosa <gnusosa AT gnusosa.net>
;; Maintainer: Carlos Ivan Sosa <gnusosa AT gnusosa.net>
;; Version: 0.2
;; Keywords: org-mode re-org jekyll
;; URL: https://github.com/gnusosa/re-org.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;;; Commentary:

;; Control your ReOrg from Emacs
;;
;; re-org.el is composed of several functions that control the
;; executable re-org ruby gem.

;; In order, for this functions to work, re-org and its requirements
;; must be found in the environment and executable path of Emacs.
;;
;; For example:
;;
;; (setenv "PATH"
;;         (concat (getenv "HOME") "/.rbenv/shims:"
;;                 (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
;; (setq exec-path
;;       (cons (concat (getenv "HOME") "/.rbenv/shims")
;;             (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(defconst re-org-version "0.2"
  "Current version of re-org.el")

(defgroup re-org nil
  "Control ReOrg from Emacs"
  :group 're-org)

(defcustom re-org-command "re-org"
  "Path to the re-org executable to use"
  :type '(string)
  :group 're-org)

(defcustom re-org-auto-open t
  "Automatically open the newly created org-file in a new buffer"
  :type '(boolean)
  :group 're-org)

;; List of options to select when re-org is called
(defvar re-org--functions-list '(("Create a new template" . re-org-new-template)
                                 ("Create a new org-file" . re-org-new)
                                 ("Quick Jekyll post" . re-org-quick-jekyll-post)
                                 ("Quick org-file" . re-org-quick))
  "alist of functions for re-org command to use when called")

(defvar re-org-mode-keymap
  (let ((re-org-mode-map (make-sparse-keymap)))
    (define-key re-org-mode-map (kbd "C-c m") 're-org)
    (define-key re-org-mode-map (kbd "C-c n") 're-org-new)
    (define-key re-org-mode-map (kbd "C-c t") 're-org-new-template)
    (define-key re-org-mode-map (kbd "C-c q") 're-org-quick)
    (define-key re-org-mode-map (kbd "C-c j") 're-org-quick-jekyll-post)
    re-org-mode-map))

;;;###autoload
(define-minor-mode re-org-mode
  "Toggle re-org mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.
Commands:
\\{re-org-mode-keymap}"
  :init-value nil
  :lighter " re-org"
  :group 're-org
  :keymap re-org-mode-keymap)

;;;###autoload
(defun re-org ()
  "Calls the selection of functions that re-org.el provides"
  (interactive)
  (let* ((opts (mapcar 'car re-org--functions-list))
         (call 
          (completing-read "What do you want re-org to do?: "
                           opts nil nil nil nil (car opts))))
    (funcall (cdr (assoc call re-org--functions-list)))))

;;;###autoload
(defun re-org-new-template ()
  "Creates a new template based on another template"
  (interactive)
  (let* ((templates (re-org--list-templates))
         (template-to-use
          (completing-read "Base Template: " templates nil nil nil nil (car templates)))
         (new-template-path
          (read-file-name "New Template's File Path: " nil nil nil nil nil)))
    (shell-command (concat re-org-command " templates --name=" template-to-use " > " new-template-path))
    (find-file new-template-path)))

;;;###autoload
(defun re-org-new ()
  "Calls the generation of a new org-file with specific arguments given"
  (interactive)
  (let* ((templates (mapcar
                     'file-name-sans-extension
                     (re-org--list-templates)))
         (template-to-use
          (completing-read "Select template to use: " templates nil nil nil nil (car templates)))
         (title
          (completing-read "Title: " nil))
         (category
          (completing-read "Category: " nil))
         (date
          (completing-read "Date: " nil))
         (layout
          (completing-read "Layout: " nil))
         (notebook
          (completing-read "Notebook: " nil))
         (path
          (read-directory-name "Path:" nil)))
    
    (re-org--create-new template-to-use title date category layout notebook path)))

;;;###autoload
(defun re-org-quick ()
  "Quickly creates a new org-file given a template and title"
  (interactive)
  (let* ((templates (mapcar
                     'file-name-sans-extension
                     (re-org--list-templates)))
         (template
          (completing-read "Select template to use: " templates nil nil nil nil (car templates)))
         (title
          (completing-read "Title: " nil nil nil nil nil nil)))
    (re-org--create-new template title)))

;;;###autoload
(defun re-org-quick-jekyll-post ()
  "Quickly creates a new jekyll-post org-file with a given title"
  (interactive)
  (let* ((title
          (completing-read "Title: " nil nil nil nil nil nil)))
    (re-org--create-new "jekyll-post" title)))

(defun re-org--create-new (template title &optional date category layout notebook path)
  "Creates a new org-file with the given arguments"
  (let ((arguments)
        (new-org-file-path))
    (setq arguments
          (apply 're-org--construct-arguments
                 (re-org--sanitize-arguments
                         (list notebook title category date layout path))))
    (setq new-org-file-path (last 
                             (split-string
                              (shell-command-to-string
                               (concat re-org-command
                                       " new "
                                       template
                                       " " arguments)))))
    (if re-org-auto-open
        (find-file (re-org--extract-clean-path new-org-file-path))
      (message "New org-file created by re-org: %s"
               (re-org--extract-clean-path new-org-file-path)))))

(defun re-org--construct-arguments
  (notebook title category date layout path)
  "Returns a string of arguments formatted for re-org-command"
  (let* ((notebook-argument)
         (title-argument)
         (category-argument)
         (date-argument)
         (layout-argument)
         (path-argument))
    
    (when notebook
      (setq notebook-argument "--notebook="))
    (when title
      (setq title-argument "--title="))
    (when category
      (setq category-argument "--category="))
    (when date
      (setq date-argument "--date="))
    (when layout
      (setq layout-argument "--layout="))
    (when path
      (setq path-argument "--path="))

    (concat notebook-argument
            notebook
            " "
            title-argument
            title
            " "
            category-argument
            category
            " "
            date-argument
            date
            " "
            layout-argument
            layout
            " "
            path-argument
            path)))

(defun re-org--sanitize-arguments (list-of-arguments)
  "Sanitizes a list of arguments, and returns the sanitized list"
  (mapcar (lambda (x) (and (not (string= x "")) x)) list-of-arguments))

(defun re-org--extract-clean-path (path-to-clean)
  "Extracts a the path of the new org file"
  (and (string-match  "`\\(.*?\\)'" (car path-to-clean))
     (match-string 1 (car path-to-clean))))

(defun re-org--list-templates ()
  "Returns a list of the templates found by the re-org command"
  (delq nil
        (mapcar (lambda (x) (and (string-match "org$" x) x))
                (split-string (shell-command-to-string
                               (concat re-org-command " templates"))))))

(provide 're-org)
;;; re-org.el ends here.

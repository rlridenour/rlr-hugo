;;; rlr-hugo.                     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Randy Ridenour

;; Author: Randy Ridenour <rlridenour@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

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

;; This is a simple package containing functions for blogging from Emacs with Hugo.

;;; Code:


(defun hugo-timestamp ()
  "Update existing date: timestamp on a Hugo post."
  (interactive)
  (save-excursion (
		   goto-char 1)
		  (re-search-forward "^#\\+date:")
		  (let ((beg (point)))
		    (end-of-line)
		    (delete-region beg (point)))
		  (insert (concat " " (format-time-string "%Y-%m-%dT%H:%M:%S")))))


;; Set a few variables and some utility functions that are used later.


(defvar hugo-directory "~/Sites/blog/" "Path to Hugo blog.")
(defvar hugo-posts-dir "content/posts/" "Relative path to posts directory.")
(defvar hugo-post-ext ".org"  "File extension of Hugo posts.")
(defvar hugo-post-template "#+TITLE: \%s\n#+draft: true\n#+tags[]: \n#+date: \n#+lastmod: \n#+mathjax: \n\n"
  "Default template for Hugo posts. %s will be replace by the post title.")

(defun hugo-make-slug (s) "Turn a string into a slug."
       (replace-regexp-in-string " " "-"  (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" "" s))))



(defun hugo-yaml-escape (s) "Escape a string for YAML."
       (if (or (string-match ":" s) (string-match "\"" s)) (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"") s))

;; Create new blog post.


  (defun hugo-draft-post (title) "Create a new Hugo blog post."
	 (interactive "sPost Title: ")
	 (let ((draft-file (concat hugo-directory hugo-posts-dir
				   (format-time-string "%Y-%m-%d-")
				   (hugo-make-slug title)
				   hugo-post-ext)))
	   (if (file-exists-p draft-file)
	       (find-file draft-file)
	     (find-file draft-file)
	     (insert (format hugo-post-template (hugo-yaml-escape title)))
	     (hugo-timestamp))))



;; This sets the draft tag to false, updates the timestamp, and saves the buffer


  (defun hugo-publish-post ()
    "Set draft to false, update the timestamp, and save."
    (interactive)
    (save-excursion
      (goto-char 1)
      (re-search-forward "^#\\+draft:")
      (let ((beg (point)))
	(end-of-line)
	(delete-region beg (point)))
      (insert " false")
      (hugo-timestamp))
    (save-buffer))

  (defmacro with-dir (DIR &rest FORMS)
    "Execute FORMS in DIR."
    (let ((orig-dir (gensym)))
      `(progn (setq ,orig-dir default-directory)
	      (cd ,DIR) ,@FORMS (cd ,orig-dir))))


;; Update last modified date.


  (defun hugo-update-lastmod ()
    "Update the `lastmod' value for a hugo org-mode buffer."
    (interactive)
    (save-excursion
      (goto-char 1)
      (re-search-forward "^#\\+lastmod:")
      (let ((beg (point)))
	(end-of-line)
	(delete-region beg (point)))
      (insert (concat " " (format-time-string "%Y-%m-%dT%H:%M:%S"))))
    (save-buffer))


;; Deploy blog.


  (defun hugo-deploy ()
    "Push changes upstream."
    (interactive)
    (with-dir hugo-directory
	      (shell-command "git add .")
	      (--> (current-time-string)
		   (concat "git commit -m \"" it "\"")
		   (shell-command it))
	      (magit-push-current-to-upstream nil)))


;; Update the last modified date of a post, save the buffer, and deploy.


  (defun hugo-org-deploy ()
    "Push changes upstream."
    (interactive)
    (hugo-update-lastmod)
    (save-buffer)
    (with-dir hugo-directory
	      (shell-command "git add .")
	      (--> (current-time-string)
		   (concat "git commit -m \"" it "\"")
		   (shell-command it))
	      (magit-push-current-to-upstream nil)))



;; Insert a tag into a Hugo post. From [[https://whatacold.io/blog/2022-10-10-emacs-hugo-blogging/][Hugo Blogging in Emacs - whatacold's space]]


  (defun hugo-select-tags ()
    "Select tags from the hugo org files in the current dir.
Note that it only extracts tags from lines like the below:
,#+tags[]: Emacs Org-mode"
    (interactive)
    ;; Move to end of tag line.
    (save-excursion
      (goto-char 1)
      (re-search-forward "^#\\+tags")
      (end-of-line)
      (let ((files (directory-files-recursively default-directory "\\.org$")))
	(let ((source (with-temp-buffer
			(while files
			  (when (file-exists-p (car files))
			    (insert-file-contents (car files)))
			  (pop files))
			(buffer-string))))
	  (save-match-data
	    (let ((pos 0)
		  matches)
	      (while (string-match "^#\\+[Tt]ags\\[\\]: \\(.+?\\)$" source pos)
		(push (match-string 1 source) matches)
		(setq pos (match-end 0)))
	      (insert
	       (completing-read
		"Insert a tag: "
		(sort
		 (delete-dups
		  (delete "" (split-string
			      (replace-regexp-in-string "[\"\']" " "
							(replace-regexp-in-string
							 "[,()]" ""
							 (format "%s" matches)))
			      " ")))
		 (lambda (a b)
		   (string< (downcase a) (downcase b))))))))))
      (insert " ")))

;; Add multiple tags to a Hugo post. I need to try to make it work with consult--read.


  (defun w/hugo--collect-tags ()
    "Collect hugo tags from the org files in the current dir.

Note that it only extracts tags from lines like the below:
,#+tags[]: Emacs Org-mode"
    (interactive)
    (let ((files (directory-files-recursively default-directory "\\.org$")))
      (let ((source (with-temp-buffer
		      (while files
			(when (file-exists-p (car files))
			  (insert-file-contents (car files)))
			(pop files))
		      (buffer-string))))
	(save-match-data
	  (let ((pos 0)
		matches)
	    (while (string-match "^#\\+[Tt]ags\\[\\]: \\(.+?\\)$" source pos)
	      (push (match-string 1 source) matches)
	      (setq pos (match-end 0)))
	    (sort
	     (delete-dups
	      (delete "" (split-string
			  (replace-regexp-in-string "[\"\']" " "
						    (replace-regexp-in-string
						     "[,()]" ""
						     (format "%s" matches)))
			  " ")))
	     (lambda (a b)
	       (string< (downcase a) (downcase b)))))))))

  (defun w/hugo-select-tags ()
    "Select tags for the current hugo post."
    (interactive)
    (ivy-read "Insert tags: "
	      (w/hugo--collect-tags)
	      :action
	      (lambda (tag)
		(insert (if (char-equal (preceding-char) 32)
			    ""
			  " ")
			tag))))

;; Insert internal links using C-c C-l.
;; From https://lucidmanager.org/productivity/create-websites-with-org-mode-and-hugo/


  ;; Follow Hugo links
  (defun org-hugo-follow (link)
    "Follow Hugo link shortcodes"
    (org-link-open-as-file
     (string-trim "{{< ref test.org >}}" "{{< ref " ">}}")))

  ;; New link type for Org-Hugo internal links
  (with-after-elpaca-init
   (org-link-set-parameters
    "hugo"
    :complete (lambda ()
		(concat "{{< ref "
			(file-name-nondirectory
			 (read-file-name "File: "))
			" >}}"))
    :follow #'org-hugo-follow))


(provide 'rlr-hugo)
;;; rlr-hugo.el ends here

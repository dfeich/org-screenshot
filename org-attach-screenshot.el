;;; org-attach-screenshot.el --- Screenshots integrated with org attachment dirs

;; Author: Derek Feichtinger <derek.feichtinger@psi.ch>
;; Keywords: org multimedia
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://github.com/dfeich/org-screenshot
;; Version: 0.9

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; allows taking screenshots from within an Emacs org buffer by using
;; the org-attach-screenshot command. The link to the file will be placed at
;; (point) and org inline images will be turned on to display it.

;; Screenshots are placed into the org entry's attachment
;; directory. If no attachment directory has been defined, the user
;; will be offered choices for creating one or using a directory of an
;; entry higher up in the hierarchy.
;;
;; The Emacs frame from which the command is issued will hide away
;; during the screenshot taking, except if a prefix argument has been
;; given (so to allow taking images of the Emacs session itself).

;; You can customize the command that is executed for taking the
;; screenshot. Look at the various customization variables.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-attach-screenshot)

;;; Code:

(require 'org-attach)
(require 'cl-lib)

;; save-mark-and-excursion in Emacs 25 works like save-excursion did before
(eval-when-compile
  (if (< emacs-major-version 25)
      (defmacro org-attach-screenshot--save-mark-and-excursion (&rest body)
        `(save-excursion ,@body))
    (defmacro org-attach-screenshot--save-mark-and-excursion (&rest body)
      `(save-mark-and-excursion ,@body))))

(defgroup org-attach-screenshot nil
  "Allows taking screenshots from within an emacs org
buffer. Screenshot files are saved in locations that are defined
by the org attachment system" :group 'org :version 24.3)

(defcustom org-attach-screenshot-command-line "import %f"
  "Contains the command line used to take a screenshot.
You need to indicate the place where the filename should be
substituted by %f" :group 'org-attach-screenshot :type 'string)

(defcustom org-attach-screenshot-dirfunction nil
  "Function generating an attachment directory name.
Will be used to generate a directory name if it is not set to nil."
  :type 'function)

(defcustom org-attach-screenshot-relative-links t
  "Configure whether to use relative filenames.
If non-nil, the screenshot links placed in the org buffer will
always be relative filenames.  If nil, the links will just be the
concatenation of the attachment dir and the filename"
  :type 'boolean :group 'org-attach-screenshot)

(defcustom org-attach-screenshot-auto-refresh 'always
  "Refresh inline image display after inserting an image.
Set this to `always' if you want to see every new image
immediately after calling `org-attach-screenshot'. Set this to
`never' if you prefere to manually refresh inline image display.
In this case `org-attach-screenshot' will always just insert the
link to the image file. Set this to `ask' if you want
`org-attach-screenshot' to ask you after every insertion if you
would like to refresh the buffer's inline images."
  :type 'symbol
  :options (list 'always 'never 'ask)
  :group 'org-attach-screenshot)

(defun org-attach-screenshot-defaultinsert (linkfilename)
  "Default function for inserting the image link into the document.
The image's filename is passed as the only argument `LINKFILENAME'."
  (insert (concat "[[file:" linkfilename "]]")))

(defcustom org-attach-screenshot-insertfunction 'org-attach-screenshot-defaultinsert
  "Function to call for the actual insertion of the image link.
This function will be called with the single argument of the
image file name. You may substitute an own function, e.g. for
naming images using org decorator features like #+NAME,
#+CAPTION, etc., or you may want to include other side effects."
  :type 'function
  :group 'org-attach-screenshot)

;;;###autoload
(defun org-attach-screenshot (prfx filename)
  "Take an area screenshot and place it in the entry's attachment directory.

The user is interactively prompted for a base FILENAME for the
screenshot.  If the name is empty, a generic name will be
generated.  If the org entry has no defined attachment directory,
the user will be offered the choice to create one through the
`org-attach-screenshot-get-attach-dir' function.

The frame invoking the function gets hidden while taking the
screenshot unless a prefix argument PRFX is passed (this allows
taking screenshots of the Emacs session itself).  If no filename
extension is provided, .png will be added.

The command for invoking the external screenshot utility can be
customized using the `org-attach-screenshot-command-line' variable.

Note that the screenshots are not stored as actual attachments
which would mean that entries for the Attachments would be
written to the PROPERTIES section of a headline in addition to
the links being already placed inside the text."
  
  (interactive (list current-prefix-arg
		     (let ((defval (format-time-string
				    "screenshot-%Y%m%d-%H%M%S.png")))
		       (read-string
			(format "Screenshot base filename (%s): "
				defval)
			nil nil
			defval))))
  (unless (file-name-extension filename)
    (setq filename (concat filename ".png")))
  (cl-assert (derived-mode-p 'org-mode) nil
	     "you must be in org mode to take a screenshot")
  (let* ((scrfilename (concat (file-name-as-directory
			       (org-attach-screenshot-get-attach-dir))
			      filename))
	 (arglst (split-string org-attach-screenshot-command-line " "))
	 (cmd (car arglst))
	 linkfilename status)
    (cl-assert (executable-find cmd) nil
	       "Cannot find executable '%s'. Please check org-attach-screenshot-command-line"
	       cmd)
    (if org-attach-screenshot-relative-links
	(setq linkfilename
	      (file-relative-name
	       scrfilename (file-name-directory
			    (or (buffer-file-name) default-directory))))
      (setq linkfilename scrfilename))
    (if (and (file-exists-p scrfilename)
	     (not (y-or-n-p (format "%s already exists. Overwrite? "
				    scrfilename))))
	(call-interactively 'org-attach-screenshot)
      ;;(insert (concat "[[file:" linkfilename "]]"))
      (funcall org-attach-screenshot-insertfunction linkfilename)
      (unless prfx (make-frame-invisible nil t))
      ;; we must canoncicalize the file name when we hand it
      ;; by call-process to the import command
      (let* ((scrpath (convert-standard-filename  (expand-file-name scrfilename)))
	     (args (mapcar (lambda (x) (replace-regexp-in-string "%f" scrpath x t t))
			   (cdr arglst))))
	(setq status (apply 'call-process cmd nil nil nil args))
	(unless prfx (make-frame-visible))
	(unless (equal status 0)
	  (error "Screenshot command exited with status %d: %s" status
		 (mapconcat 'identity (cons cmd args) " ")) )
	(message "wrote screenshot to %s" scrpath))
      (when (or (eq org-attach-screenshot-auto-refresh 'always)
                (and (eq org-attach-screenshot-auto-refresh 'ask)
                     (y-or-n-p "Refresh inline images? ")))
        (org-display-inline-images nil t)))))

(defun org-attach-screenshot-get-attach-dir ()
  "Return or create the current entry's attachment directory.
Also offers the option of using an attachment directory defined
higher up in the org headline hierarchy, even though attachment
inheritance has not been turned on by ATTACH_DIR_INHERIT."
  (require 'org-attach)
  (if (derived-mode-p 'org-mode)
      (let
	  ((dir (org-attach-dir)) (tmpbuf "*Screenshot Attach*")
	   (inhdir (org-entry-get nil "ATTACH_DIR" t))
	   (funcdir (when org-attach-screenshot-dirfunction
		      (funcall org-attach-screenshot-dirfunction)))
	   c)
	(unless dir
	  (org-attach-screenshot--save-mark-and-excursion
	   (save-window-excursion
	     (with-output-to-temp-buffer tmpbuf
	       (princ (concat
		       "The current org entry has no attachment directory

Select command:

s       Set a specific attachment directory for this org entry
c       have org create a standard directory name for this entry"
		       (when funcdir
			 (concat "
d       use name from user defined function:"  "
          " funcdir))
		       (if inhdir (concat "
i       use attachment directory of ancestor entry:" "
          " inhdir)))))
	     (org-fit-window-to-buffer (get-buffer-window tmpbuf))
	     (message "Select command:")
	     (setq c (read-char-exclusive))
	     (and (get-buffer tmpbuf) (kill-buffer tmpbuf))))
	  (cond
	   ((memq c '(?s ?\C-s)) (call-interactively
				  'org-attach-set-directory)
	    (setq dir (org-attach-dir t)))
	   ((memq c '(?c ?\C-c)) (setq dir (org-attach-dir t)))
	   ((and (memq c '(?i ?\C-i)) inhdir) (setq dir inhdir))
	   ((and (memq c '(?d ?\C-d))
		 funcdir) (progn (org-entry-put nil "ATTACH_DIR" funcdir)
		 (setq dir (org-attach-dir t))))
	   (t (error "No such attachment command %c" c)) ))
	;; we return the directory name and create it if necessary
	dir)
    (error "This is not org-mode, but %s" major-mode) nil))

(provide 'org-attach-screenshot)
;;; org-attach-screenshot.el ends here

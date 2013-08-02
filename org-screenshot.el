;;; org-screenshot.el --- screenshots integrated with org attachment dirs

;; Author: Derek Feichtinger <derek.feichtinger@psi.ch>
;; Version: 0.1

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

;; Commentary:

;; allows taking of screenshots from within an emacs org buffer. The
;; link to the file will be placed at (point) and org inline images
;; will be turned on to display it.

;; Screenshots are placed into the org entry's attachment
;; directory. If no attachment directory has been defined, the user
;; will be offered choices for creating one or using a directory of an
;; entry higher up in the hierarchy.
;;
;; The emacs frame from which the command is issued will hide away
;; during the screenshot taking, except if a prefix argument has been
;; given (so to allow taking images of the emacs session itself).

;; Requires the "import" command from the ImageMagick suite

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-screenshot)


(provide 'org-screenshot)

(defun org-screenshot (prfx filename)
  "take an area screenshot and place it in the entry's attachment
  directory

The user is interactively prompted for a base file name for the
screenshot. If the name is empty, a generic name will be
generated.  If the org entry has no defined attachment directory,
the user will be offered the choice to create one through the
`my-get-org-attach-dir' function.

The frame invoking the function gets hidden while taking the
screenshot unless a prefix argument is passed (this allows
taking screenshots of the emacs session itself).
If no filename extension is provided, .png will be added."
  (interactive "P\nsScreenshot base filename: ")
  (if (equal filename "")
      (setq filename (format-time-string "screenshot-%Y%m%d-%H%M%S.png")))
  (unless (file-name-extension filename)
    (setq filename (concat filename ".png")))
  (if (equal major-mode 'org-mode)
      (let ((scrfilename (concat (file-name-as-directory
				       (org-screenshot-get-attach-dir))
		     filename)))
	      (if (and (file-exists-p scrfilename)
		       (not (y-or-n-p (format "%s already exists. Overwrite?"
					      scrfilename))))
		  (call-interactively 'org-screenshot)
		(insert (concat "[[file:" scrfilename "]]"))
		;; this would cause asynchronous execution: (screenshot-take)
		(unless prfx (make-frame-invisible nil t))
		(call-process "import" nil nil nil scrfilename)
		(unless prfx (make-frame-visible))
		(org-display-inline-images nil t)))
    (error "you are not in org mode"))
)

(defun org-screenshot-get-attach-dir ()
  "Return the current entry's attachment directory or let the
user create one. Also offers to use an attachment directory
defined higher up in the hierarchy, even though inheritance has
not been turned on"
  (require 'org-attach)
  (if (equal major-mode 'org-mode)
    (let 
	((dir (org-attach-dir)) (tmpbuf "*Screenshot Attach*")
	 (inhdir (org-entry-get nil "ATTACH_DIR" t)) c)
      (unless dir
	(save-excursion
	  (save-window-excursion 
	    (with-output-to-temp-buffer tmpbuf
	      (princ (concat
		      "The current org entry has no attachment directory

Select command:

s       Set a specific attachment directory for this org entry
c       have org create a standard directory name for this entry"
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
	 ((and  (memq c '(?i ?\C-i)) inhdir) (setq dir inhdir))
	 (t (error "No such attachment command %c" c)) )) 
      ;; we return the directory name and create it if necessary
      dir)
    (error "This is not org-mode, but %s" major-mode) nil))

;; org-screenshot.el ends here

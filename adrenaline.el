;;; adrenaline.el --- Replace the modeline with something fast and thin -*- lexical-binding: t; -*-

;; Author: Craig Jennings <c@cjennings.net>
;; Maintainer: Craig Jennings <c@cjennings.net>
;; URL: https://github.com/cjennings/adrenaline

;; Inspired by Feebleline by Benjamin Lindqvist
;; https://github.com/tautologyclub/feebleline

;; Package-Version: 0.5
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.5
;; SPDX-License-Identifier: GPL-3.0-or-later
;; This file is not part of GNU Emacs.

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

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'subr-x)

;; tell byte-compiler this is a valid function defined at runtime
(declare-function tramp-tramp-file-p "tramp")

;; ------------------------------ Custom Variables -----------------------------

(defgroup adrenaline nil
  "Adrenaline customizations."
  :prefix "adrenaline-"
  :group 'convenience)

(defcustom adrenaline-timer-interval 0.5
  "Refresh interval of adrenaline."
  :type  'float
  :group 'adrenaline)

;; ----------------------------- Internal Variables ----------------------------

(defvar adrenaline--home-dir nil
  "The user's home directory.
This variable is used to abbreviate file paths in adrenaline messages.
Set during adrenaline-mode activation.")

(defvar-local adrenaline--msg-timer nil
  "Timer object for mode line updates in the current buffer.")

(defvar-local adrenaline--original-mode-line-format nil
  "Backup storage for the previous mode line format in the current buffer.")

(defvar adrenaline--original-window-divider-setting nil
  "Previous window divider setting before adrenaline mode was enabled.
This variable is used to restore the old setting when adrenaline mode is
disabled.")

(defvar adrenaline--last-error-shown nil
  "The last error that was displayed by adrenaline mode.
This variable is used to prevent the same error from being displayed
repeatedly.")

(defvar adrenaline--minibuf " *Minibuf-0*"
  "Buffer name used by adrenaline for displaying messages.
This buffer is primarily used to overwrite the echo area.")

;; -------------------------- Line And Column Element --------------------------

(defface adrenaline-line-column-face
  '((t :inherit 'default :foreground "#E37464"))
  "The default face used to display version control project names and branches."
  :group 'adrenaline)

(defun adrenaline-line-column-info ()
  "Return a formatted string displaying the current line and column number.
The line number is formatted to occupy 5 spaces, and the
column number exactly 3 spaces, separated by a colon.
For example, the string for line 120 and column 15 would be '  120:15 '."
  (let ((line-column-info
		 (format "%5s:%-3s"
				 (format "%s" (line-number-at-pos)) (current-column))))
	(propertize line-column-info
                :face 'adrenaline-line-column-face)))

;; ----------------------------- Major Mode Element ----------------------------

(defface adrenaline-major-mode-face
  '((t :inherit 'default :foreground "#704214"))  ; sepia-like color
  "Face used to display the buffer's major mode."
  :group 'adrenaline)

(defun adrenaline-major-mode-info ()
  "Return a string representing the `major-mode', with '-mode' removed.
If there is no major mode, return an empty string."
  (let ((mode-name (format-mode-line mode-name)))
	(propertize (if (and mode-name
						 (string-suffix-p "-mode" mode-name))
					(substring mode-name 0 -5)
				  mode-name)
                :face 'adrenaline-major-mode-face)))

;; ----------------- Buffer Name Or File Name With Path Element ----------------

(defface adrenaline-directory-face
  '((t :inherit 'default :foreground "#5E4D37"))
  "The default face used to display directory paths."
  :group 'adrenaline)

(defface adrenaline-buffer-face
  '((t :inherit 'default :foreground "#2B61B6"))
  "The default face used to display buffer names."
  :group 'adrenaline)

(defun adrenaline-directory-info ()
  "Return the directory if buffer is displaying a file."
  (when (buffer-file-name)
	(replace-regexp-in-string
     (concat "^" adrenaline--home-dir) "~"
	 (propertize default-directory :face 'adrenaline-directory-face))))

(defun adrenaline-buffer-info ()
  "Return the buffer name, or the file name if buffer is associated with a file."
  (let ((filename (if (buffer-file-name)
			  (file-name-nondirectory (buffer-file-name))
			(buffer-name))))
    (propertize filename :face 'adrenaline-buffer-face)))

(defun adrenaline-buffer-modified-star ()
  "Return a star if buffer was modified."
  (when (and (buffer-file-name) (buffer-modified-p))
	(propertize "*" :face 'adrenaline-buffer-face)))

;; --------------------------- VC Information Element --------------------------

(defface adrenaline-vc-info-face
  '((t :inherit 'default :foreground "#94E151"))
  "The default face used to display version control project names and branches."
  :group 'adrenaline)

(defun adrenaline-vc-project-name ()
  "Return project name if exists."
  (when-let ((proj (project-current)))
	(file-name-nondirectory
	 (directory-file-name (cdr proj)))))

(defun adrenaline-vc-branch ()
  "Return current version control branch prefixed with the branch symbol.
If file is remote or not in a repo, show nothing. Currently works with git only."
  (if-let ((fname (buffer-file-name))
		   (branch
			(and fname
				 (not (file-remote-p fname))
				 (not (tramp-tramp-file-p fname))
				 (vc-backend fname)
				 (vc-state fname)
				 (car (vc-git-branches)))))
	  (string-trim (concat "" branch))
	""))

(defun adrenaline-vc-info ()
  "Return the concatenation of the project name and the git branch.
When the project name doesn't exist, return nil."
  (let ((project-name (adrenaline-vc-project-name))
        (branch (adrenaline-vc-branch)))
	(if project-name (concat project-name " " branch)
	  nil)))

;; -------------------------------- Element List -------------------------------

(defcustom adrenaline-element-list
  '((adrenaline-major-mode-info)
	(adrenaline-directory-info :post "")
	(adrenaline-buffer-info    :post "")
    (adrenaline-buffer-modified-star)
    (adrenaline-line-column-info)
	(adrenaline-vc-info        :align right))
  "List of functions or elements to display in the echo area.
Each element is a function giving a string to display directly or a list where:
- The first element is the function.
- The other elements are keyword-value pairs for advanced formatting options.

Available keywords are:
- :pre, an optional string to insert before the function output.
- :post, an optional string to insert after the function output.
- :fmt, a format string, defaults to \"%s\".
- :face, a optional face to apply to the whole string.
- :align, an optional symbol specifying alignment (\='left or \='right).
Example:
:align \='right will align the output string to the right of the echo area.

Note: an empty space is automatically inserted after each element. To avoid
this, the element should end with :post \"\""
  :type  '(repeat sexp)
  :group 'adrenaline)

;; ---------------------------- Echo Area Insertion ----------------------------

(defmacro adrenaline-append-msg-function (&rest b)
  "Macro for adding B to the adrenaline mode-line, at the end."
  `(add-to-list 'adrenaline-element-list ,@b t (lambda (x y) nil)))

(defmacro adrenaline-prepend-msg-function (&rest b)
  "Macro for adding B to the adrenaline mode-line, at the beginning."
  `(add-to-list 'adrenaline-element-list ,@b nil (lambda (x y) nil)))

(defun adrenaline--insert-ignore-errors ()
  "Insert stuff into the echo area, ignoring potential errors."
  (unless (current-message)
    (condition-case err (adrenaline--insert)
      (error (unless (equal adrenaline--last-error-shown err)
               (setq adrenaline--last-error-shown err)
			   (message (format "Adrenaline error: %s" err)))))))

(defun adrenaline--force-insert ()
  "Insert stuff into the echo area even if it's displaying something."
  (condition-case nil (adrenaline--clear-echo-area)
	(error nil)))

(cl-defun adrenaline--insert-func (func &key pre (post " ")
										(fmt "%s") (align 'left))
  "Format an element of \='adrenaline-element-list\=' based on its properties.
- FUNC is the function used to generate the message.
- PRE is an optional string to insert before the function output.
- POST is an optional string to insert after the function output,
  defaults to a single space.
- FMT is a format string, defaults to \"%s\".
- ALIGN is an optional symbol specifying alignment (\='left or \='right),
  defaults to \='left.

Returns a pair with align setting and the resulting string."
  (list align
		(let* ((msg (apply func nil))
			   (string (concat pre (format fmt msg) post)))
		  (if msg
			  (if face
				  (propertize string 'face face)
				string)
			""))))

(defun adrenaline--insert ()
  "Insert stuff into the mini buffer."
  (unless (current-message)
	(let ((left ())
		  (right ()))
      (dolist (idx adrenaline-element-list)
        (let* ((fragment (apply 'adrenaline--insert-func idx))
			   (align (car fragment))
			   (string (cadr fragment)))
		  (cond
		   ((eq align 'left)
			(push string left))
		   ((eq align 'right)
			(push string right))
		   (t
			(push string left))))) ; default to left if not specified
      (with-current-buffer adrenaline--minibuf
		(erase-buffer)
		(let* ((left-string (string-join (reverse left)))
			   (message-truncate-lines t)
			   (max-mini-window-height 1)
			   (right-string (string-join (reverse right)))
			   (free-space (- (frame-width)
							  (length left-string) (length right-string)))
			   (padding (make-string (max 0 free-space) ?\ )))
		  (insert (concat left-string
						  (if right-string (concat padding
												   right-string)))))))))

(defun adrenaline--clear-echo-area ()
  "Erase the echo area."
  (with-current-buffer adrenaline--minibuf
	(erase-buffer)))

;; ------------------------------ Adrenaline Mode ------------------------------

;;;###autoload
(define-minor-mode adrenaline-mode
  "Replace modeline with a slimmer proxy."
  :require 'adrenaline
  :global t
  (if adrenaline-mode
	  ;; Activation:
	  (progn
        (setq adrenaline--home-dir (expand-file-name "~"))
        (setq adrenaline--original-mode-line-format mode-line-format)
        (setq adrenaline--msg-timer
              (run-with-timer 0 adrenaline-timer-interval
                              'adrenaline--insert-ignore-errors))
        (adrenaline-appearance-settings-on)
		(add-function :after after-focus-change-function
                      'adrenaline--insert-ignore-errors))
	;; Deactivation:
    (window-divider-mode adrenaline--original-window-divider-setting)
	(set-face-attribute 'mode-line nil :height 1.0)
    (setq-default mode-line-format adrenaline--original-mode-line-format)
	(walk-windows (lambda (window)
					(with-selected-window window
					  (setq mode-line-format
                            adrenaline--original-mode-line-format)))
				  nil t)
    (cancel-timer adrenaline--msg-timer)
	(remove-function after-focus-change-function
                     'adrenaline--insert-ignore-errors)

	(force-mode-line-update)
	(redraw-display)
    (adrenaline--clear-echo-area)))

;; ----------------------- Adrenaline Appearance Settings ----------------------

(defun adrenaline-appearance-settings-on ()
  "The appearance settings for adrenaline.
Set window divider to 1 pixel and place at bottom. Save original settings to be
restored once adrenaline mode is deactivated."
  (setq window-divider-default-bottom-width 1
		window-divider-default-places (quote bottom-only))
  (setq adrenaline--original-window-divider-setting window-divider-mode)
  (window-divider-mode 1)
  (setq-default mode-line-format nil)
  (walk-windows (lambda (window)
				  (with-selected-window window
					(setq mode-line-format nil)))
				nil t))

;; ---------------------------- Disable And Reenable ---------------------------

(defun adrenaline-disable ()
  "Disable \='adrenaline-mode\='.
This is meant to be used in a hook, before issuing a conflicting command."
  (when adrenaline-mode
    (cancel-timer adrenaline--msg-timer)
	(remove-function after-focus-change-function
                     'adrenaline--insert-ignore-errors)))

(defun adrenaline-reenable ()
  "Re-enable \='adrenaline-mode\='.
This is meant to be used in a hook, after issuing a conflicting command."
  (when adrenaline-mode
    (setq adrenaline--msg-timer
          (run-with-timer 0 adrenaline-timer-interval
                          'adrenaline--insert-ignore-errors))
	(add-function :after after-focus-change-function
                  'adrenaline--insert-ignore-errors)))

(provide 'adrenaline)
;;; adrenaline.el ends here

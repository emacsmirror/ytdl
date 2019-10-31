;;; ydl4e.el --- Emacs Interface for youtube-dl -*- lexical-binding: t -*-

;; Copyright (C) 2019 Arnaud Hoffmann

;; Author: Arnaud Hoffmann <tuedachu@gmail.com>
;; Maintainer: Arnaud Hoffmann <tuedachu@gmail.com>
;; URL: https://gitlab.com/tuedachu/ydl4e
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.1") (emms "5.2"))
;; Keywords: comm, emulations, multimedia

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ydl4e is an Emacs-based interface for youtube-dl, written in
;; emacs-lisp.
;;
;; youtube-dl is a command-line program to download videos from
;; YouTube.com and a few more sites.  More information, at
;; https://github.com/ytdl-org/youtube-dl/blob/master/README.md#readme.
;;
;; * Setup
;;
;; Add "(require 'ydl4e)" to your "init.el" file
;;
;; Further customization can be found in the documentation online.

;;; Code:

(require 'eshell)

(defcustom ydl4e-music-folder
  (expand-file-name "~/music")
  "Folder where music will be downloaded."
  :group 'ydl4e
  :type '(string))

(defcustom ydl4e-video-folder
  nil
  "Folder where videos will be downloaded."
  :group 'ydl4e
  :type '(string))

(defcustom ydl4e-download-folder
  (expand-file-name "~/Downloads")
  "Default folder for downloads."
  :group 'ydl4e
  :type '(string))

(defvar ydl4e-download-extra-args
  nil
  "Default extra arguments for the default download type 'Downloads'.")

(defvar ydl4e-music-extra-args
  '("-x" "--audio-format" "mp3")
  "Default extra arguments for the default download type 'Music'.")

(defvar ydl4e-video-extra-args
  nil
  "Default extra arguments for the default download type 'Videoss'.")

(defvar ydl4e-download-types
  '(("Downloads" "d" ydl4e-download-folder ydl4-download-extra-args)
    ("Music"  "m" ydl4e-music-folder ydl4e-music-extra-args)
    ("Videos" "v"  ydl4e-video-folder ydl4-video-extra-args))
  "List of destination folders.

Each element is a list '(FIELD-NAME SHORTCUT
ABSOLUTE-PATH-TO-FOLDER FORMAT EXTRA-COMMAND-LINE-ARGS) where:
FIELD-NAME is a string; SHORTCUT is a string (only one
character); ABSOLUTE-PATH-TO-FOLDER is the absolute path to the
given folder; FILE-FORMAT is a file format accepted by
youtube-dl;EXTRA-COMMAND-LINE-ARGS is extra command line
arguments for youtube-dl.")

(defcustom ydl4e-always-query-default-filename
  nil
  "Whether to always query default-filename to youtube-dl.

 Note that this operation may take a few seconds."
  :group 'ydl4e
  :type 'boolean)

(defcustom ydl4e-always-ask-delete-confirmation
  t
  "Whether to ask for confirmation when deleting a file."
  :group 'ydl4e
  :type 'boolean)

(defvar ydl4e-last-downloaded-file-name
  nil)

(defun ydl4e-add-field-in-download-type-list (field-name keyboard-shortcut path-to-folder extra-args)
  "Add new field in the list of download types `ydl4e-download-types'.

Add element '(FIELD-NAME KEYBOARD-SHORTCUT PATH-TO-FOLDER
EXTRA-ARGS) to list ofdownload types.

NOTE that the PATH-TO-FOLDER and EXTRA-ARGS can be symbols."
  (add-to-list 'ydl4e-download-types `(,field-name ,keyboard-shortcut ,path-to-folder ,extra-args)))


(defun ydl4e-run-youtube-dl-eshell(url destination-folder filename &optional extra-ydl-args)
  "Run youtube-dl in a new eshell buffer.

URL is the url of the video to download.  DESTINATION-FOLDER is
the folder where the video will be downloaded.  FILENAME is the
relative path (from DESTINATION-FOLDER) of the output file.

Optional argument EXTRA-YDL-ARGS is the list of extra arguments
to youtube-dl.

This opration is asynchronous."

  (let ((eshell-buffer-name "*youtube-dl*"))
    (split-window-right)
    (windmove-right)
    (eshell)
    (when (eshell-interactive-process)
      (eshell t))
    (eshell-interrupt-process)
    (insert (format "cd '%s' && youtube-dl " destination-folder)
            url
            " -o " (concat
                    filename
                    ".%(ext)s"))
    (when extra-ydl-args
      (insert " " (mapconcat 'identity extra-ydl-args " ")))
    (eshell-send-input)
    (windmove-left)))

(defun ydl4e-run-youtube-dl-sync(url absolute-filename &optional extra-ydl-args)
  "Run youtube-dl in a new eshell buffer.

URL is the url of the video to download.  ABSOLUTE-FILENAME is
the absolute path where the file will be saved.

Optional argument EXTRA-YDL-ARGS is the list of extra arguments
to youtube-dl.

This opration is synchronous."
  (let ((error-message (with-temp-buffer
                         (apply #'call-process "youtube-dl" nil t nil url
                                "-o" (concat absolute-filename
                                             ".%(ext)s")
                                extra-ydl-args)
                         (buffer-string))))
    (when (string-match-p "ERROR" error-message)
      error-message)))

(defun ydl4e-get-default-filename (url)
  "Get default filename from webserver.

Query the dafult-filename of URL using '--get-filename' argument
of youtube-dl."
  (if ydl4e-always-query-default-filename
      (with-temp-buffer
        (call-process "youtube-dl" nil '(t nil) nil url "--get-filename" "--restrict-filenames")
        (goto-char (point-min))
        (search-forward ".")
        (buffer-substring-no-properties (line-beginning-position)
                                        (1- (point))))
    nil))

(defun ydl4e-get-download-type()
  "Query download type in mini-buffer.

  User can choose candidates from the elements of
  `ydl4e-download-types' whose ABSOLUTE-PATH-TO-FOLDER is not nil.

  Returns (destination-folder file-format extra-args)."

  (let ((user-input (read-char-choice (concat (propertize "Destination folder:" 'face 'default)
                                              (mapconcat (lambda(x)
                                                           (when (ydl4e-eval-field (nth 2 x))
                                                             (let ((destination (nth 0 x))
                                                                   (letter-shortcut (ydl4e-eval-field (nth 1 x))))
                                                               (concat " "
                                                                       destination
                                                                       "["
                                                                       (propertize letter-shortcut 'face 'font-lock-warning-face)
                                                                       "]"))))
                                                         ydl4e-download-types
                                                         ""))
                                      (mapcar (lambda(x)
                                                (when (ydl4e-eval-field (nth 2 x))
                                                  (aref (ydl4e-eval-field(nth 1 x)) 0)))
                                              ydl4e-download-types))))
    (mapcan (lambda(x)
              (when (= (aref (ydl4e-eval-field (nth 1 x)) 0) user-input)
                `(,(nth 2 x) ,(nth 3 x))))
            ydl4e-download-types)))

(defun ydl4e-eval-field (field)
  "Return the value of FIELD.

Test whether FIELD is a symbol.  If it is a symbol, returns the
value of the symbol."
  (if (symbolp field)
      (symbol-value field)
    field))

(defun ydl4e-eval-list (list)
  "Evaluate all elements of LIST.

Test whether each element is a symbol.  If it is a symbol,
returns the value of the symbol."
  (mapcar (lambda(arg)
            (ydl4e-eval-field arg))
          list))

(defun ydl4e-get-filename (destination-folder url)
  "Query a filename in mini-buffer.

If `ydl4e-always-query-default-filename' is t, then the default
value in the mini-buffer is the default filename of the URL.

Returns a valid string:
- no '/' in the filename
- The filename does not exist yet in DESTINATION-FOLDER."

  (let* ((prompt "Filename [no extension]: ")
         (default-filename (ydl4e-get-default-filename url))
         (filename (read-from-minibuffer prompt
                                         default-filename)))
    (while (or (search "/" filename)
               (and (file-exists-p destination-folder)
                    (let ((filename-completed (file-name-completion filename destination-folder)))
                      (when filename-completed
                        (string= (file-name-nondirectory filename)
                                 (substring filename-completed
                                            0
                                            (search "." filename-completed)))))))
      (minibuffer-message (if (search "/" filename)
                              "Filename cannot contain '/'!"
                            "Filename already exist in the destination folder (eventually with a different extension)!"))
      (setq filename (read-from-minibuffer prompt
                                           default-filename)))
    (setq filename (concat destination-folder
                           "/"
                           filename))))

(defun ydl4e-destination-folder-exist? (destination-folder)
  "Test if DESTINATION-FOLDER exists.

If DESTINATION-FOLDER exists, then returns t.

Else, query user if DESTINATION-FOLDER should be created. If so,
creates DESTINATION-FOLDER and returns t. Else, returns nil."
  (if (file-exists-p destination-folder)
      t
    (if (y-or-n-p (concat "Directory '"
                          destination-folder
                          "' does not exist. Do you want to create it?"))
        (progn
          (make-directory destination-folder)
          t)
      (minibuffer-message "Operation aborted..."))))

(defun ydl4e-download (&optional url absolute-destination-path extra-ydl-args)
  "Download file from a web server using youtube-dl.

If URL is given as argument, then download file from URL.  Else
download the file from the url stored in `current-ring'.

Download the file from URL and place it at
ABSOLUTE-DESTINATION-PATH if provided.  Else, query the download
type and use the associated destination folder.  See
`ydl4e-add-field-in-download-type-list'.

If provided, use EXTRA-YDL-ARGS as extra arguments for youtue-dl
executable.  ETXRA-YDL-ARGS is alist of strings.  Else, query the
download type, and use the associated extra arguments.  See
`ydl4e-add-field-in-download-type-list'."
  (interactive)
  (let* ((url (or url
                  (current-kill 0)))
         (dl-type (when (not (and absolute-destination-path
                                  extra-ydl-args))
                    (ydl4e-get-download-type)))
         (destination-folder (ydl4e-eval-field (nth 0 dl-type)))
         (filename (or absolute-destination-path
                       (ydl4e-get-filename  destination-folder url)))
         (extra-ydl-args (or extra-ydl-args
                             (ydl4e-eval-list (nth 2 dl-type))))
         (run-youtube-dl? nil))
    (setq run-youtube-dl? (ydl4e-destination-folder-exist? destination-folder))
    (when run-youtube-dl?
      (ydl4e-run-youtube-dl-eshell url
                                   (file-name-directory filename)
                                   (file-name-nondirectory filename)
                                   extra-ydl-args))))


(defun ydl4e-download-open(&optional url)
  "Download file from a web server using youtube-dl and open it in `emms'.

If URL is given as argument, then download file from URL.  Else
download the file from the url stored in `current-ring'."
  (interactive)
  (let* ((url (or url
                  (current-kill 0)))
         (dl-type (ydl4e-get-download-type))
         (destination-folder (ydl4e-eval-field (nth 0 dl-type)))
         (extra-ydl-args (ydl4e-eval-list (nth 2 dl-type)))
         (abs-filename (ydl4e-get-filename destination-folder url))
         (run-youtube-dl? nil))
    (setq run-youtube-dl? (ydl4e-destination-folder-exist? destination-folder))
    (when run-youtube-dl?
      (ydl4e-run-youtube-dl-sync url
                                 abs-filename
                                 extra-ydl-args)
      (setq ydl4e-last-downloaded-file-name (concat destination-folder
                                                    "/"
                                                    (file-name-completion (file-name-nondirectory abs-filename)
                                                                          destination-folder)))
      (minibuffer-message (concat "Opening file "
                                  (file-name-completion (file-name-nondirectory abs-filename)
                                                        destination-folder)))
      (if (require 'emms)
          (emms-play-file ydl4e-last-downloaded-file-name)
        (minibuffer-message "emms is not installed!")))))


(defun ydl4e-delete-last-downloaded-file ()
  "Delete the last file downloaded though ydl4e."
  (interactive)
  (if ydl4e-always-ask-delete-confirmation
      (when (= ?y (read-char-choice (concat "Are you sure you want to delete the file '"
                                            ydl4e-last-downloaded-file-name
                                            "'"
                                            "? [y/n]") '(?y?n)))
        (delete-file ydl4e-last-downloaded-file-name)
        (minibuffer-message "Deleting file..."))
    (delete-file ydl4e-last-downloaded-file-name)
    (minibuffer-message "Deleting file...")))

(provide 'ydl4e)
;;; ydl4e.el ends here

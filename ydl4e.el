;;; ydl4e.el --- Emacs Interface for youtube-dl -*- lexical-binding: t -*-

;; Copyright (C) 2019 Arnaud Hoffmann

;; Author: Arnaud Hoffmann <tuedachu@gmail.com>
;; Maintainer: Arnaud Hoffmann <tuedachu@gmail.com>
;; URL: https://gitlab.com/tuedachu/ydl4e
;; Version: 1.2.1
;; Package-Requires: ((emacs "24.3") (async "1.9.4"))
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
(require 'async)

(defvar ydl4e-version
  "1.2.1"
  "Version of ydl4e.")

(defcustom ydl4e-music-folder
  nil
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
  "Default extra arguments for the default download type 'Videos'.")

(defvar ydl4e-download-types
  '(("Downloads" "d" ydl4e-download-folder ydl4e-download-extra-args)
    ("Music"  "m" ydl4e-music-folder ydl4e-music-extra-args)
    ("Videos" "v"  ydl4e-video-folder ydl4e-video-extra-args))
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

(defcustom ydl4e-media-player
  "mpv"
  "Media player to use to open videos.

Default is 'mpv'.
Used by `ydl4e-download-open'."
  :group 'ydl4e
  :type 'string)

(defcustom ydl4e-message-start
  "[ydl4e] "
  "String that starts all mini-buffer messages from `ydl4e'.
Default value is '[ydl4e] '."
  :group 'ydl4e
  :type 'string)

(defcustom ydl4e-mode-line
  t
  "Show `ydl4e' information in EMACS mode line."
  :group 'ydl4e
  :type 'boolean)

(defvar ydl4e-last-downloaded-file-name
  nil
  "Path to the last file downloaded by `ydl4e'.")

(defvar ydl4e-download-in-progress
  0
  "Number of `ydl4e' downloads currently in progress.")

(defvar ydl4e-mode-line-string
  ""
  "`ydl4e' global mode string.")

(defvar ydl4e-mode-line-initialized?
  nil
  "Whether `ydl4e' has been initialized or not.

See `ydl4e--eval-mode-line-string'.")

(defun ydl4e--eval-mode-line-string(increment)
  "Evaluate `ydl4e' global mode string.

- Increment (or decrement) `ydl4e-download-in-progress' based on
INCREMENT value.
- If needed, add `ydl4e-mode-line-string' to `global-mode-string'.
- Update `ydl4e-mode-line-string'."
  (setq ydl4e-download-in-progress (+ ydl4e-download-in-progress increment))
  (when ydl4e-mode-line
    ;; Add `ydl4e-mode-line-string' to `global-mode-string' only if needed.
    (unless ydl4e-mode-line-initialized?
      (let ((l global-mode-string)
            (ydl4e-string-found nil))
        (while (and (not ydl4e-string-found)
                    l)
          (when (equal (car l) ydl4e-mode-line-string)
            (setq ydl4e-string-found t))
          (setq l (cdr l)))
        (unless ydl4e-string-found
          (setq global-mode-string (append global-mode-string
                                           '("" ydl4e-mode-line-string))
                ydl4e-mode-line-initialized? t))))
    (setq ydl4e-mode-line-string (if (> ydl4e-download-in-progress 0)
                                     (format "[ydl4e] downloading %s file(s)..." ydl4e-download-in-progress)
                                   ""))))

(defun ydl4e-add-field-in-download-type-list (field-name keyboard-shortcut path-to-folder extra-args)
  "Add new field in the list of download types `ydl4e-download-types'.

  Add element '(FIELD-NAME KEYBOARD-SHORTCUT PATH-TO-FOLDER
                           EXTRA-ARGS) to list ofdownload types.

  NOTE that the PATH-TO-FOLDER and EXTRA-ARGS can be symbols."
  (add-to-list 'ydl4e-download-types `(,field-name ,keyboard-shortcut ,path-to-folder ,extra-args)))


(defun ydl4e--run-youtube-dl-eshell(url destination-folder filename &optional extra-ydl-args)
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
                    ".%(ext)s")
            (if extra-ydl-args
                (insert " " (mapconcat #'identity extra-ydl-args " "))
              ""))
    (eshell-send-input)
    (windmove-left)))

(defun ydl4e-get-default-filename (url)

(defun ydl4e--get-default-filename (url)
  "Get default filename from webserver.

  Query the dafult-filename of URL using '--get-filename' argument
  of youtube-dl."
  (if ydl4e-always-query-default-filename
      (when (y-or-n-p (concat ydl4e-message-start
                              "Do you want to query the default filename? (This might take a few seconds)"))
        (with-temp-buffer
          (call-process "youtube-dl" nil '(t nil) nil url "--get-filename" "--restrict-filenames")
          (goto-char (point-min))
          (search-forward ".")
          (buffer-substring-no-properties (line-beginning-position)
                                          (1- (point)))))
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

  (let* ((prompt (concat ydl4e-message-start
                         "Filename [no extension]: "))
         (default-filename (ydl4e--get-default-filename url))
         (filename (read-from-minibuffer prompt
                                         default-filename)))
    (while (or (cl-search "/" filename)
               (and (file-exists-p destination-folder)
                    (let ((filename-completed (file-name-completion
                                               filename destination-folder)))
                      (when filename-completed
                        (string= (file-name-nondirectory filename)
                                 (substring filename-completed
                                            0
                                            (cl-search "."
                                                       filename-completed)))))))
      (minibuffer-message (concat ydl4e-message-start
                                  (if (cl-search "/" filename)
                                      "Filename cannot contain '/'!"
                                    "Filename already exist in the destination folder (eventually with a different extension)!")))
      (setq filename (read-from-minibuffer prompt
                                           default-filename)))
    (setq filename (concat destination-folder
                           "/"
                           filename))))

(defun ydl4e--destination-folder-exists-p (destination-folder)
  "Test if DESTINATION-FOLDER exists.

  If DESTINATION-FOLDER exists, then returns t.

  Else, query user if DESTINATION-FOLDER should be created.  If so,
  creates DESTINATION-FOLDER and returns t. Else, returns nil."
  (if (file-exists-p destination-folder)
      t
    (if (y-or-n-p (concat "Directory '"
                          destination-folder
                          "' does not exist. Do you want to create it?"))
        (progn
          (make-directory destination-folder)
          t)
      (minibuffer-message (concat ydl4e-message-start
                                  "Operation aborted...")))))

(defun ydl4e--open-file-in-media-player (filename)
  "Open FILENAME in `ydl4e-media-player'."
  (start-process-shell-command ydl4e-media-player
                               nil
                               (concat ydl4e-media-player
                                       " "
                                       filename)))

(defun ydl4e--download-async (url filename extra-ydl-args &optional finish-function)
  "Asynchronously download URL into FILENAME.

  Extra arguments to youtube-dl can be provided with EXTRA-YDL-ARGS.

  FINISH-FUNCTION is a function that is executed once the file is
  downloaded.  It takes a single argument (file-path)."
  (ydl4e--eval-mode-line-string 1)
  (async-start
   (lambda ()
     (apply #'call-process "youtube-dl" nil nil nil
            url
            "-o" (concat filename
                         ".%(ext)s")
            extra-ydl-args)
     (let ((file-path nil)
           (youtube-dl-extensions
            '("3gp" "aac" "flv" "m4a" "mp3" "mp4" "ogg" "wav" "webm" "mkv")))
       (while (and (not (when file-path
                          (file-exists-p file-path)))
                   youtube-dl-extensions)
         (setq file-path (concat filename
                                 "."
                                 (car youtube-dl-extensions))
               youtube-dl-extensions (cdr youtube-dl-extensions)))
       file-path))

   (lambda (file-path)
     (ydl4e--async-download-finished file-path)
     (when finish-function
       (funcall finish-function file-path)))))

(defun ydl4e--async-download-finished (filename)
  "Generic function run after download is completed.

FILENAME is the absolute path of the file downloaded by
  `ydl4e-download-async'.  See `ydl4e-download-async' for more
  details."
  (setq ydl4e-last-downloaded-file-name filename)
  (ydl4e--eval-mode-line-string -1)
  (message (concat ydl4e-message-start
                   "Video downloaded: "
                   filename)))

(defun ydl4e--get-args ()
  "Query user for ydl4e arguments."
  (let* ((url (read-from-minibuffer (concat ydl4e-message-start
                                            "URL: ")
                                    (current-kill 0)))
         (dl-type (ydl4e-get-download-type))
         (destination-folder (ydl4e-eval-field (nth 0 dl-type)))
         (filename (ydl4e-get-filename  destination-folder url))
         (extra-ydl-args (ydl4e-eval-list (nth 2 dl-type)))
         (run-youtube-dl? (ydl4e--destination-folder-exists-p destination-folder)))
    (list url filename extra-ydl-args run-youtube-dl?)))


(defun ydl4e-download-eshell ()
  "Download file from a web server using youtube-dl in eshell.

  Download the file from the provided url into the appropriate
  folder location.  Query the download type and use the associated
  destination folder and extra arguments, see
  `ydl4e-add-field-in-download-type-list'."
  (interactive)
  (let* ((out (ydl4e--get-args))
         (url (nth 0 out))
         (filename (nth 1 out))
         (extra-ydl-args (nth 2 out))
         (run-youtube-dl? (nth 3 out)))
    (when run-youtube-dl?
      (ydl4e--run-youtube-dl-eshell url
                                    (file-name-directory filename)
                                    (file-name-nondirectory filename)
                                    extra-ydl-args))))

(defun ydl4e-download ()
  "Download asynchronously file from a web server using youtube-dl."
  (interactive)
  (let* ((out (ydl4e--get-args))
         (url (nth 0 out))
         (filename (nth 1 out))
         (extra-ydl-args (nth 2 out))
         (run-youtube-dl? (nth 3 out)))
    (when run-youtube-dl?
      (ydl4e-download-async url
                            filename
                            extra-ydl-args))))

(defun ydl4e-download-open ()
  "Download file from a web server using and open it with `ydl4e-media-player'.

  If URL is given as argument, then download file from URL.  Else
  download the file from the url stored in `current-ring'."
  (interactive)
  (let* ((out (ydl4e--get-args))
         (url (nth 0 out))
         (filename (nth 1 out))
         (extra-ydl-args (nth 2 out))
         (run-youtube-dl? (nth 3 out)))
    (unless ydl4e-media-player
      (minibuffer-message (concat ydl4e-message-start
                                  "No media player is set up. See `ydl4e-media-player'.")))
    (unless (executable-find ydl4e-media-player)
      (minibuffer-message (concat ydl4e-message-start
                                  "Program "
                                  ydl4e-media-player
                                  " cannot be found. Operation aborted.")))
    (when run-youtube-dl?
      (ydl4e--download-async url
                             filename
                             extra-ydl-args
                             'ydl4e--open-file-in-media-player))))

(defun ydl4e-open-last-downloaded-file ()
  "Open the last downloaded file in `ydl4e-media-player'.

  The last downloaded file is stored in `ydl4e-last-downloaded-file-name'."
  (interactive)
  (ydl4e--open-file-in-media-player ydl4e-last-downloaded-file-name))

(defun ydl4e-delete-last-downloaded-file ()
  "Delete the last file downloaded though ydl4e."
  (interactive)
  (when (or (not ydl4e-always-ask-delete-confirmation)
            (y-or-n-p (concat ydl4e-message-start
                              "Are you sure you want to delete the file '"
                              ydl4e-last-downloaded-file-name
                              "'"
                              "?")))
    (delete-file ydl4e-last-downloaded-file-name)
    (minibuffer-message (concat ydl4e-message-start
                                "Deleting file..."))))

(provide 'ydl4e)
;;; ydl4e.el ends here

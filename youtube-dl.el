;;; youtube-dl.el --- Emacs Interface for youtube-dl -*- lexical-binding: t -*-

;; Copyright (C) 2019 Arnaud Hoffmann

;; Author: Arnaud Hoffmann <tuedachu@gmail.com>
;; Maintainer: Arnaud Hoffmann <tuedachu@gmail.com>
;; URL: https://gitlab.com/tuedachu/youtube-dl.el
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
;; youtube-dl.el is an Emacs-based interface for youtube-dl, written in
;; emacs-lisp.
;;
;; youtube-dl is a command-line program to download videos from
;; YouTube.com and a few more sites.  More information, at
;; https://github.com/ytdl-org/youtube-dl/blob/master/README.md#readme.
;;
;; * Setup
;;
;; Add "(require 'youtube-dl)" to your "init.el" file
;;
;; Further customization can be found in the documentation online.

;;; Code:

(require 'eshell)
(require 'async)

(defgroup youtube-dl
  nil
  "Emacs interface for youtube-dl."
  :group 'external)

(defvar youtube-dl-version
  "1.2.1"
  "Version of youtube-dl.")

(defcustom youtube-dl-music-folder
  nil
  "Folder where music will be downloaded."
  :group 'youtube-dl
  :type '(string))

(defcustom youtube-dl-video-folder
  nil
  "Folder where videos will be downloaded."
  :group 'youtube-dl
  :type '(string))

(defcustom youtube-dl-download-folder
  (expand-file-name "~/Downloads")
  "Default folder for downloads."
  :group 'youtube-dl
  :type '(string))

(defcustom youtube-dl-always-query-default-filename
  nil
  "Whether to always query default-filename to youtube-dl.

 Note that this operation may take a few seconds."
  :group 'youtube-dl
  :type 'boolean)

(defcustom youtube-dl-always-ask-delete-confirmation
  t
  "Whether to ask for confirmation when deleting a file."
  :group 'youtube-dl
  :type 'boolean)

(defcustom youtube-dl-media-player
  "mpv"
  "Media player to use to open videos.

Default is 'mpv'.
Used by `youtube-dl-download-open'."
  :group 'youtube-dl
  :type '(string))

(defcustom youtube-dl-message-start
  "[youtube-dl] "
  "String that starts all mini-buffer messages from `youtube-dl'.
Default value is '[ytdl] '."
  :group 'youtube-dl
  :type '(string))

(defcustom youtube-dl-mode-line
  t
  "Show `youtube-dl' information in EMACS mode line."
  :group 'youtube-dl
  :type 'boolean)

(defcustom youtube-dl-max-mini-buffer-download-type-entries
  5
  "Maximum number of download types displayed in the minibuffer
  with `read-char-choice'. If the number of download types is
  greater than
  `youtube-dl-max-mini-buffer-download-type-entries', then user
  queries will be done through `completing-read'."
  :group 'youtube-dl
  :type '(string))


(defvar youtube-dl-download-extra-args
  nil
  "Default extra arguments for the default download type 'Downloads'.")

(defvar youtube-dl-music-extra-args
  '("-x" "--audio-format" "mp3")
  "Default extra arguments for the default download type 'Music'.")

(defvar youtube-dl-video-extra-args
  nil
  "Default extra arguments for the default download type 'Videos'.")

(defvar youtube-dl-download-types
  '(("Downloads" "d" youtube-dl-download-folder youtube-dl-download-extra-args)
    ("Music"  "m" youtube-dl-music-folder youtube-dl-music-extra-args)
    ("Videos" "v"  youtube-dl-video-folder youtube-dl-video-extra-args))
  "List of destination folders.

Each element is a list '(FIELD-NAME SHORTCUT
ABSOLUTE-PATH-TO-FOLDER EXTRA-COMMAND-LINE-ARGS) where:
FIELD-NAME is a string; SHORTCUT is a string (only one
character); ABSOLUTE-PATH-TO-FOLDER is the absolute path to the
given folder; EXTRA-COMMAND-LINE-ARGS is extra command line
arguments for youtube-dl.")

(defvar youtube-dl--last-downloaded-file-name
  nil
  "Path to the last file downloaded by `youtube-dl'.")

(defvar youtube-dl--download-in-progress
  0
  "Number of `youtube-dl' downloads currently in progress.")

(defvar youtube-dl--mode-line-string
  ""
  "`youtube-dl' global mode string.")

(defvar youtube-dl--mode-line-initialized?
  nil
  "Whether `youtube-dl' has been initialized or not.

See `youtube-dl--eval-mode-line-string'.")


(defun youtube-dl--eval-mode-line-string(increment)
  "Evaluate `youtube-dl' global mode string.

- Increment (or decrement) `youtube-dl--download-in-progress' based on
INCREMENT value.
- If needed, add `youtube-dl--mode-line-string' to `global-mode-string'.
- Update `youtube-dl--mode-line-string'."
  (setq youtube-dl--download-in-progress (+ youtube-dl--download-in-progress increment))
  (when youtube-dl-mode-line
    ;; Add `youtube-dl--mode-line-string' to `global-mode-string' only if needed.
    (unless youtube-dl--mode-line-initialized?
      (let ((l global-mode-string)
            (youtube-dl-string-found nil))
        (while (and (not youtube-dl-string-found)
                    l)
          (when (equal (car l) youtube-dl--mode-line-string)
            (setq youtube-dl-string-found t))
          (setq l (cdr l)))
        (unless youtube-dl-string-found
          (setq global-mode-string (append global-mode-string
                                           '("" youtube-dl--mode-line-string))
                youtube-dl--mode-line-initialized? t))))
    (setq youtube-dl--mode-line-string (if (> youtube-dl--download-in-progress 0)
                                           (format "[ytdl %s]" youtube-dl--download-in-progress)
                                         ""))))


(defun youtube-dl-add-field-in-download-type-list (field-name keyboard-shortcut path-to-folder extra-args)
  "Add new field in the list of download types `youtube-dl-download-types'.

  Add element '(FIELD-NAME KEYBOARD-SHORTCUT PATH-TO-FOLDER
                           EXTRA-ARGS) to list ofdownload types.

  NOTE that the PATH-TO-FOLDER and EXTRA-ARGS can be symbols."
  (add-to-list 'youtube-dl-download-types `(,field-name ,keyboard-shortcut ,path-to-folder ,extra-args)))


(defun youtube-dl--run-youtube-dl-eshell(url destination-folder filename &optional extra-ydl-args)
  "Run youtube-dl in a new eshell buffer.

  URL is the url of the video to download.  DESTINATION-FOLDER is
  the folder where the video will be downloaded.  FILENAME is the
  relative path (from DESTINATION-FOLDER) of the output file.

  Optional argument EXTRA-YDL-ARGS is the list of extra arguments
  to youtube-dl.

  This opration is asynchronous."

  (let ((eshell-buffer-name "*youtube-dl*"))
    (eshell)
    (when (eshell-interactive-process)
      (eshell t))
    (eshell-interrupt-process)
    (insert (concat "cd "
                    (shell-quote-argument destination-folder)
                    " && youtube-dl "
                    (shell-quote-argument url)
                    " -o "
                    (shell-quote-argument filename)
                    ".%(ext)s"
                    (when extra-ydl-args
                      (concat " "
                              (mapconcat #'shell-quote-argument
                                         extra-ydl-args
                                         " ")))))
    (eshell-send-input)))


(defun youtube-dl--get-default-filename (url)
  "Get default filename from webserver.

  Query the dafult-filename of URL using '--get-filename' argument
  of youtube-dl."
  (if youtube-dl-always-query-default-filename
      (when (y-or-n-p (concat youtube-dl-message-start
                              "Do you want to query the default filename? (This might take a few seconds)"))
        (with-temp-buffer
          (call-process "youtube-dl" nil '(t nil) nil url "--get-filename" "--restrict-filenames")
          (goto-char (point-min))
          (search-forward ".")
          (buffer-substring-no-properties (line-beginning-position)
                                          (1- (point)))))
    nil))


(defun youtube-dl--get-download-type()
  "Query download type in mini-buffer.

  User can choose candidates from the elements of
  `youtube-dl-download-types' whose ABSOLUTE-PATH-TO-FOLDER is not nil.

  Returns (destination-folder extra-args)."

  (let* ((use-completing-read? (> (length youtube-dl-download-types)
                                  youtube-dl-max-mini-buffer-download-type-entries))
         (user-input (if use-completing-read?
                         (completing-read "Choose a destination folder:"
                                          (mapcar (lambda (dl-type)
                                                    (if (youtube-dl--eval-field (nth 2 dl-type))
                                                        (nth 0 dl-type)
                                                      ""))
                                                  youtube-dl-download-types))
                       (read-char-choice (concat (propertize "Destination folder:" 'face 'default)
                                                 (mapconcat (lambda(x)
                                                              (when (youtube-dl--eval-field (nth 2 x))
                                                                (let ((destination (nth 0 x))
                                                                      (letter-shortcut (youtube-dl--eval-field (nth 1 x))))
                                                                  (concat " "
                                                                          destination
                                                                          "["
                                                                          (propertize letter-shortcut 'face 'font-lock-warning-face)
                                                                          "]"))))
                                                            youtube-dl-download-types
                                                            ""))
                                         (mapcar (lambda(x)
                                                   (when (youtube-dl--eval-field (nth 2 x))
                                                     (aref (youtube-dl--eval-field(nth 1 x)) 0)))
                                                 youtube-dl-download-types)))))

    (mapcan (lambda(x)
              (when (if use-completing-read?
                        (string= (nth 0 x) user-input)
                      (= (aref (youtube-dl--eval-field (nth 1 x)) 0) user-input))
                `(,(nth 2 x) ,(nth 3 x))))
            youtube-dl-download-types)))


(defun youtube-dl--eval-field (field)
  "Return the value of FIELD.

  Test whether FIELD is a symbol.  If it is a symbol, returns the
  value of the symbol."
  (if (symbolp field)
      (symbol-value field)
    field))


(defun youtube-dl--eval-list (list)
  "Evaluate all elements of LIST.

  Test whether each element is a symbol.  If it is a symbol,
  returns the value of the symbol."
  (mapcar (lambda(arg)
            (youtube-dl--eval-field arg))
          list))


(defun youtube-dl-get-filename (destination-folder url)
  "Query a filename in mini-buffer.

  If `youtube-dl-always-query-default-filename' is t, then the default
  value in the mini-buffer is the default filename of the URL.

  Returns a valid string:
  - no '/' in the filename
  - The filename does not exist yet in DESTINATION-FOLDER."

  (let* ((prompt (concat youtube-dl-message-start
                         "Filename [no extension]: "))
         (default-filename (youtube-dl--get-default-filename url))
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
      (minibuffer-message (concat youtube-dl-message-start
                                  (if (cl-search "/" filename)
                                      "Filename cannot contain '/'!"
                                    "Filename already exist in the destination folder (eventually with a different extension)!")))
      (setq filename (read-from-minibuffer prompt
                                           default-filename)))
    (setq filename (concat destination-folder
                           "/"
                           filename))))


(defun youtube-dl--destination-folder-exists-p (destination-folder)
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
      (minibuffer-message (concat youtube-dl-message-start
                                  "Operation aborted...")))))


(defun youtube-dl--open-file-in-media-player (filename)
  "Open FILENAME in `youtube-dl-media-player'."
  (start-process-shell-command youtube-dl-media-player
                               nil
                               (concat youtube-dl-media-player
                                       " "
                                       filename)))


(defun youtube-dl--download-async (url filename extra-ydl-args &optional finish-function)
  "Asynchronously download URL into FILENAME.

  Extra arguments to youtube-dl can be provided with EXTRA-YDL-ARGS.

  FINISH-FUNCTION is a function that is executed once the file is
  downloaded.  It takes a single argument (file-path)."
  (youtube-dl--eval-mode-line-string 1)
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
     (youtube-dl--async-download-finished file-path)
     (when finish-function
       (funcall finish-function file-path)))))


(defun youtube-dl--async-download-finished (filename)
  "Generic function run after download is completed.

FILENAME is the absolute path of the file downloaded by
  `youtube-dl--download-async'.  See `youtube-dl--download-async'
  for more details."
  (setq youtube-dl--last-downloaded-file-name filename)
  (youtube-dl--eval-mode-line-string -1)
  (message (concat youtube-dl-message-start
                   "Video downloaded: "
                   filename)))


(defun youtube-dl--get-args ()
  "Query user for youtube-dl arguments."
  (let* ((url (read-from-minibuffer (concat youtube-dl-message-start
                                            "URL: ")
                                    (current-kill 0)))
         (dl-type (youtube-dl--get-download-type))
         (destination-folder (youtube-dl--eval-field (nth 0 dl-type)))
         (filename (youtube-dl-get-filename  destination-folder url))
         (extra-ydl-args (youtube-dl--eval-list (youtube-dl--eval-field (nth 1 dl-type))))
         (run-youtube-dl? (youtube-dl--destination-folder-exists-p destination-folder)))
    (list url filename extra-ydl-args run-youtube-dl?)))


(defun youtube-dl-download-eshell ()
  "Download file from a web server using youtube-dl in eshell.

  Download the file from the provided url into the appropriate
  folder location.  Query the download type and use the associated
  destination folder and extra arguments, see
  `youtube-dl-add-field-in-download-type-list'."
  (interactive)
  (let* ((out (youtube-dl--get-args))
         (url (nth 0 out))
         (filename (nth 1 out))
         (extra-ydl-args (nth 2 out))
         (run-youtube-dl? (nth 3 out)))
    (when run-youtube-dl?
      (youtube-dl--run-youtube-dl-eshell url
                                         (file-name-directory filename)
                                         (file-name-nondirectory filename)
                                         extra-ydl-args))))


(defun youtube-dl-download ()
  "Download asynchronously file from a web server using youtube-dl."
  (interactive)
  (let* ((out (youtube-dl--get-args))
         (url (nth 0 out))
         (filename (nth 1 out))
         (extra-ydl-args (nth 2 out))
         (run-youtube-dl? (nth 3 out)))
    (when run-youtube-dl?
      (youtube-dl--download-async url
                                  filename
                                  extra-ydl-args))))


(defun youtube-dl-download-open ()
  "Download file from a web server using and open it with `youtube-dl-media-player'.

  If URL is given as argument, then download file from URL.  Else
  download the file from the url stored in `current-ring'."
  (interactive)
  (let* ((out (youtube-dl--get-args))
         (url (nth 0 out))
         (filename (nth 1 out))
         (extra-ydl-args (nth 2 out))
         (run-youtube-dl? (nth 3 out)))
    (unless youtube-dl-media-player
      (minibuffer-message (concat youtube-dl-message-start
                                  "No media player is set up. See `youtube-dl-media-player'.")))
    (unless (executable-find youtube-dl-media-player)
      (minibuffer-message (concat youtube-dl-message-start
                                  "Program "
                                  youtube-dl-media-player
                                  " cannot be found. Operation aborted.")))
    (when run-youtube-dl?
      (youtube-dl--download-async url
                                  filename
                                  extra-ydl-args
                                  'youtube-dl--open-file-in-media-player))))


(defun youtube-dl-open-last-downloaded-file ()
  "Open the last downloaded file in `youtube-dl-media-player'.

  The last downloaded file is stored in `youtube-dl--last-downloaded-file-name'."
  (interactive)
  (youtube-dl--open-file-in-media-player youtube-dl--last-downloaded-file-name))

(defun youtube-dl-delete-last-downloaded-file ()
  "Delete the last file downloaded though youtube-dl."
  (interactive)
  (when (or (not youtube-dl-always-ask-delete-confirmation)
            (y-or-n-p (concat youtube-dl-message-start
                              "Are you sure you want to delete the file '"
                              youtube-dl--last-downloaded-file-name
                              "'"
                              "?")))
    (delete-file youtube-dl--last-downloaded-file-name)
    (minibuffer-message (concat youtube-dl-message-start
                                "Deleting file..."))))


(provide 'youtube-dl)
;;; youtube-dl.el ends here




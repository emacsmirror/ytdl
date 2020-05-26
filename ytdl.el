;;; ytdl.el --- Emacs Interface for youtube-dl -*- lexical-binding: t -*-

;; Copyright (C) 2019 Arnaud Hoffmann

;; Author: Arnaud Hoffmann <tuedachu@gmail.com>
;; Maintainer: Arnaud Hoffmann <tuedachu@gmail.com>
;; URL: https://gitlab.com/tuedachu/ytdl
;; Version: 1.2.2
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
;; ytdl.el is an Emacs-based interface for youtube-dl, written in
;; emacs-lisp.
;;
;; youtube-dl is a command-line program to download videos from
;; YouTube.com and a few more sites.  More information, at
;; https://github.com/ytdl-org/ytdl/blob/master/README.md#readme.
;;
;; * Setup
;;
;; Add "(require 'ytdl)" to your "init.el" file
;;
;; Further customization can be found in the documentation online.

;;; Code:

(require 'eshell)
(require 'async)

(defgroup ytdl
  nil
  "Emacs interface for ytdl."
  :group 'external)

(defvar ytdl-version
  "1.2.1"
  "Version of ytdl.")

(defcustom ytdl-music-folder
  nil
  "Folder where music will be downloaded."
  :group 'ytdl
  :type '(string))

(defcustom ytdl-video-folder
  nil
  "Folder where videos will be downloaded."
  :group 'ytdl
  :type '(string))

(defcustom ytdl-download-folder
  (expand-file-name "~/Downloads")
  "Default folder for downloads."
  :group 'ytdl
  :type '(string))

(defcustom ytdl-always-query-default-filename
  nil
  "Whether to always query default-filename to ytdl.

 Note that this operation may take a few seconds."
  :group 'ytdl
  :type 'boolean)

(defcustom ytdl-always-ask-delete-confirmation
  t
  "Whether to ask for confirmation when deleting a file."
  :group 'ytdl
  :type 'boolean)

(defcustom ytdl-media-player
  "mpv"
  "Media player to use to open videos.

Default is 'mpv'.
Used by `ytdl-download-open'."
  :group 'ytdl
  :type '(string))

(defcustom ytdl-message-start
  "[ytdl] "
  "String that starts all mini-buffer messages from `ytdl'.
Default value is '[ytdl] '."
  :group 'ytdl
  :type '(string))

(defcustom ytdl-mode-line
  t
  "Show `ytdl' information in EMACS mode line."
  :group 'ytdl
  :type 'boolean)

(defcustom ytdl-max-mini-buffer-download-type-entries
  5
  "Maximum number of download types displayed in the minibuffer.

If the number of download types is greater than
`ytdl-max-mini-buffer-download-type-entries', then user queries
will be done through `completing-read' instead of
`read-char-choice'."
  :group 'ytdl
  :type '(string))


(defvar ytdl-download-extra-args
  nil
  "Default extra arguments for the default download type 'Downloads'.")

(defvar ytdl-music-extra-args
  '("-x" "--audio-format" "mp3")
  "Default extra arguments for the default download type 'Music'.")

(defvar ytdl-video-extra-args
  nil
  "Default extra arguments for the default download type 'Videos'.")

(defvar ytdl-download-types
  '(("Downloads" "d" ytdl-download-folder ytdl-download-extra-args)
    ("Music"  "m" ytdl-music-folder ytdl-music-extra-args)
    ("Videos" "v"  ytdl-video-folder ytdl-video-extra-args))
  "List of destination folders.

Each element is a list '(FIELD-NAME SHORTCUT
ABSOLUTE-PATH-TO-FOLDER EXTRA-COMMAND-LINE-ARGS) where:
FIELD-NAME is a string; SHORTCUT is a string (only one
character); ABSOLUTE-PATH-TO-FOLDER is the absolute path to the
given folder; EXTRA-COMMAND-LINE-ARGS is extra command line
arguments for ytdl.")

(defvar ytdl--last-downloaded-file-name
  nil
  "Path to the last file downloaded by `ytdl'.")

(defvar ytdl--download-in-progress
  0
  "Number of `ytdl' downloads currently in progress.")

(defvar ytdl--mode-line-string
  ""
  "`ytdl' global mode string.")

(defvar ytdl--mode-line-initialized?
  nil
  "Whether `ytdl' has been initialized or not.

See `ytdl--eval-mode-line-string'.")


(defun ytdl--eval-mode-line-string(increment)
  "Evaluate `ytdl' global mode string.

- Increment (or decrement) `ytdl--download-in-progress' based on
INCREMENT value.

- If needed, add `ytdl--mode-line-string' to
  `global-mode-string'.

- Update `ytdl--mode-line-string'."
  (setq ytdl--download-in-progress (+ ytdl--download-in-progress increment))
  (when ytdl-mode-line
    ;; Add `ytdl--mode-line-string' to `global-mode-string' only if needed.
    (unless ytdl--mode-line-initialized?
      (let ((l global-mode-string)
            (ytdl-string-found nil))
        (while (and (not ytdl-string-found)
                    l)
          (when (equal (car l) ytdl--mode-line-string)
            (setq ytdl-string-found t))
          (setq l (cdr l)))
        (unless ytdl-string-found
          (setq global-mode-string (append global-mode-string
                                           '("" ytdl--mode-line-string))
                ytdl--mode-line-initialized? t))))
    (setq ytdl--mode-line-string (if (> ytdl--download-in-progress 0)
                                     (format "[ytdl %s]" ytdl--download-in-progress)
                                   ""))))


(defun ytdl-add-field-in-download-type-list (field-name keyboard-shortcut path-to-folder extra-args)
  "Add new field in the list of download types `ytdl-download-types'.

Add element '(FIELD-NAME KEYBOARD-SHORTCUT PATH-TO-FOLDER
                           EXTRA-ARGS) to list ofdownload types.

NOTE that the PATH-TO-FOLDER and EXTRA-ARGS can be symbols."
  (add-to-list 'ytdl-download-types `(,field-name ,keyboard-shortcut ,path-to-folder ,extra-args)))


(defun ytdl--run-ytdl-eshell(url destination-folder filename &optional extra-ydl-args)
  "Run ytdl in a new eshell buffer.

URL is the url of the video to download.  DESTINATION-FOLDER is
the folder where the video will be downloaded.  FILENAME is the
relative path (from DESTINATION-FOLDER) of the output file.

Optional argument EXTRA-YDL-ARGS is the list of extra arguments
to youtube-dl.

This opration is asynchronous."

  (let ((eshell-buffer-name "*ytdl*"))
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


(defun ytdl--get-default-filename (url)
  "Get default filename from webserver.

Query the dafult-filename of URL using '--get-filename' argument
of ytdl."
  (if ytdl-always-query-default-filename
      (when (y-or-n-p (concat ytdl-message-start
                              "Do you want to query the default filename? "
                              "(This might take a few seconds)"))
        (with-temp-buffer
          (call-process "youtube-dl" nil '(t nil) nil url "--get-filename" "--restrict-filenames")
          (goto-char (point-min))
          (search-forward ".")
          (buffer-substring-no-properties (line-beginning-position)
                                          (1- (point)))))
    nil))


(defun ytdl--get-download-type()
  "Query download type in mini-buffer.

User can choose candidates from the elements of
`ytdl-download-types' whose ABSOLUTE-PATH-TO-FOLDER is not nil.

Returns (destination-folder extra-args)."

  (let* ((use-completing-read? (> (length ytdl-download-types)
                                  ytdl-max-mini-buffer-download-type-entries))
         (user-input (if use-completing-read?
                         (completing-read "Choose a destination folder:"
                                          (mapcar (lambda (dl-type)
                                                    (if (ytdl--eval-field (nth 2 dl-type))
                                                        (nth 0 dl-type)
                                                      ""))
                                                  ytdl-download-types))
                       (read-char-choice (concat (propertize "Destination folder:" 'face 'default)
                                                 (mapconcat (lambda(x)
                                                              (when (ytdl--eval-field (nth 2 x))
                                                                (let ((destination (nth 0 x))
                                                                      (letter-shortcut (ytdl--eval-field (nth 1 x))))
                                                                  (concat " "
                                                                          destination
                                                                          "["
                                                                          (propertize letter-shortcut 'face 'font-lock-warning-face)
                                                                          "]"))))
                                                            ytdl-download-types
                                                            ""))
                                         (mapcar (lambda(x)
                                                   (when (ytdl--eval-field (nth 2 x))
                                                     (aref (ytdl--eval-field(nth 1 x)) 0)))
                                                 ytdl-download-types)))))

    (mapcan (lambda(x)
              (when (if use-completing-read?
                        (string= (nth 0 x) user-input)
                      (= (aref (ytdl--eval-field (nth 1 x)) 0) user-input))
                `(,(nth 2 x) ,(nth 3 x))))
            ytdl-download-types)))


(defun ytdl--eval-field (field)
  "Return the value of FIELD.

Test whether FIELD is a symbol.  If it is a symbol, returns the
value of the symbol."
  (if (symbolp field)
      (symbol-value field)
    field))


(defun ytdl--eval-list (list)
  "Evaluate all elements of LIST.

Test whether each element is a symbol.  If it is a symbol,
returns the value of the symbol."
  (mapcar (lambda(arg)
            (ytdl--eval-field arg))
          list))


(defun ytdl-get-filename (destination-folder url)
  "Query a filename in mini-buffer.

If `ytdl-always-query-default-filename' is t, then the
defaultvalue in the mini-buffer is the default filename of the
URL.

Returns a valid string:
- no '/' in the filename
- The filename does not exist yet in DESTINATION-FOLDER."

  (let* ((prompt (concat ytdl-message-start
                         "Filename [no extension]: "))
         (default-filename (ytdl--get-default-filename url))
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
      (minibuffer-message (concat ytdl-message-start
                                  (if (cl-search "/" filename)
                                      "Filename cannot contain '/'!"
                                    "Filename already exist in the destination folder (eventually with a different extension)!")))
      (setq filename (read-from-minibuffer prompt
                                           default-filename)))
    (setq filename (concat destination-folder
                           "/"
                           filename))))


(defun ytdl--destination-folder-exists-p (destination-folder)
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
      (minibuffer-message (concat ytdl-message-start
                                  "Operation aborted...")))))


(defun ytdl--open-file-in-media-player (filename)
  "Open FILENAME in `ytdl-media-player'."
  (start-process-shell-command ytdl-media-player
                               nil
                               (concat ytdl-media-player
                                       " "
                                       filename)))


(defun ytdl--download-async (url filename extra-ydl-args &optional finish-function)
  "Asynchronously download URL into FILENAME.

Extra arguments to ytdl can be provided with EXTRA-YDL-ARGS.

FINISH-FUNCTION is a function that is executed once the file is
downloaded.  It takes a single argument (file-path)."
  (ytdl--eval-mode-line-string 1)
  (async-start
   (lambda ()
     (with-temp-buffer
       (apply #'call-process "youtube-dl" nil t nil
              url
              "-o" (concat filename
                           ".%(ext)s")
              extra-ydl-args)
       (goto-char (point-min))
       (if (search-forward-regexp "^ERROR:" nil t nil)
           (progn
             (beginning-of-line)
             (buffer-substring-no-properties (line-beginning-position)
                                             (line-end-position)))
         (let ((file-path nil)
               (ytdl-extensions
                '("3gp" "aac" "flv" "m4a" "mp3" "mp4" "ogg" "wav" "webm" "mkv")))
           (while (and (not (when file-path
                              (file-exists-p file-path)))
                       ytdl-extensions)
             (setq file-path (concat filename
                                     "."
                                     (car ytdl-extensions))
                   ytdl-extensions (cdr ytdl-extensions)))
           file-path))))

   (lambda (response)
     (if (string-match "^ERROR" response)
         (progn
           (ytdl--eval-mode-line-string -1)
           (message (concat ytdl-message-start
                            response)))
       (ytdl--async-download-finished response)
       (when finish-function
         (funcall finish-function response))))))


(defun ytdl--async-download-finished (filename)
  "Generic function run after download is completed.

FILENAME is the absolute path of the file downloaded by
`ytdl--download-async'.  See `ytdl--download-async' for more
details."
  (setq ytdl--last-downloaded-file-name filename)
  (ytdl--eval-mode-line-string -1)
  (message (concat ytdl-message-start
                   "Video downloaded: "
                   filename)))


(defun ytdl--get-args ()
  "Query user for ytdl arguments."
  (let* ((url (read-from-minibuffer (concat ytdl-message-start
                                            "URL: ")
                                    (current-kill 0)))
         (dl-type (ytdl--get-download-type))
         (destination-folder (ytdl--eval-field (nth 0 dl-type)))
         (filename (ytdl-get-filename  destination-folder url))
         (extra-ydl-args (ytdl--eval-list (ytdl--eval-field (nth 1 dl-type))))
         (run-ytdl? (ytdl--destination-folder-exists-p destination-folder)))
    (list url filename extra-ydl-args run-ytdl?)))


(defun ytdl-download-eshell ()
  "Download file from a web server using ytdl in eshell.

Download the file from the provided url into the appropriate
folder location.  Query the download type and use the associated
destination folder and extra arguments, see
`ytdl-add-field-in-download-type-list'."
  (interactive)
  (let* ((out (ytdl--get-args))
         (url (nth 0 out))
         (filename (nth 1 out))
         (extra-ydl-args (nth 2 out))
         (run-ytdl? (nth 3 out)))
    (when run-ytdl?
      (ytdl--run-ytdl-eshell url
                             (file-name-directory filename)
                             (file-name-nondirectory filename)
                             extra-ydl-args))))


(defun ytdl-download ()
  "Download asynchronously file from a web server."
  (interactive)
  (let* ((out (ytdl--get-args))
         (url (nth 0 out))
         (filename (nth 1 out))
         (extra-ydl-args (nth 2 out))
         (run-ytdl? (nth 3 out)))
    (when run-ytdl?
      (ytdl--download-async url
                            filename
                            extra-ydl-args))))


(defun ytdl-download-open ()
  "Download file from a web server using and open it.

If URL is given as argument, then download file from URL.  Else
download the file from the url stored in `current-ring'.

The file is opened with `ytdl-media-player'."
  (interactive)
  (let* ((out (ytdl--get-args))
         (url (nth 0 out))
         (filename (nth 1 out))
         (extra-ydl-args (nth 2 out))
         (run-ytdl? (nth 3 out)))
    (unless ytdl-media-player
      (minibuffer-message (concat ytdl-message-start
                                  "No media player is set up. See `ytdl-media-player'.")))
    (unless (executable-find ytdl-media-player)
      (minibuffer-message (concat ytdl-message-start
                                  "Program "
                                  ytdl-media-player
                                  " cannot be found. Operation aborted.")))
    (when run-ytdl?
      (ytdl--download-async url
                            filename
                            extra-ydl-args
                            'ytdl--open-file-in-media-player))))


(defun ytdl-open-last-downloaded-file ()
  "Open the last downloaded file in `ytdl-media-player'.

The last downloaded file is stored in
`ytdl--last-downloaded-file-name'."
  (interactive)
  (ytdl--open-file-in-media-player ytdl--last-downloaded-file-name))

(defun ytdl-delete-last-downloaded-file ()
  "Delete the last file downloaded though ytdl."
  (interactive)
  (when (or (not ytdl-always-ask-delete-confirmation)
            (y-or-n-p (concat ytdl-message-start
                              "Are you sure you want to delete the file '"
                              ytdl--last-downloaded-file-name
                              "'"
                              "?")))
    (delete-file ytdl--last-downloaded-file-name)
    (minibuffer-message (concat ytdl-message-start
                                "Deleting file..."))))


(provide 'ytdl)
;;; ytdl.el ends here

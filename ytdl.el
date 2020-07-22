;;; ytdl.el --- Emacs Interface for youtube-dl -*- lexical-binding: t -*-

;; Copyright (C) 2020 Arnaud Hoffmann

;; Author: Arnaud Hoffmann <tuedachu@gmail.com>
;; Maintainer: Arnaud Hoffmann <tuedachu@gmail.com>
;; URL: https://gitlab.com/tuedachu/ytdl
;; Version: 1.3.0
;; Package-Requires: ((emacs "26.1") (async "1.9.4") (transient "0.2.0"))
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
;; YouTube and a few more sites.  More information at
;; https://yt-dl.org.
;;
;; youtube-dl supports many more sites: PeeTube, BBC, IMDB,
;; InternetVideoArchive (non-exhaustive list)
;;
;; * Setup
;;
;; Add "(require 'ytdl)" to your "init.el" file.
;;
;; Further customization can be found in the documentation online.

;;; Code:

(require 'eshell)
(require 'async)
(require 'transient)
(require 'cl-lib)
(require 'time-stamp)


(defgroup ytdl
  nil
  "Emacs interface for ytdl."
  :group 'external)

(defvar ytdl-version
  "1.3.0"
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
  "Default extra arguments for the default download type 'Downloads'.

Expected value is a list of extra command line arguments for
youtube-dl.")

(defvar ytdl-music-extra-args
  '("-x" "--audio-format" "mp3")
  "Default extra arguments for the default download type 'Music'.

Expected value is a list of extra command line arguments for
youtube-dl.")

(defvar ytdl-video-extra-args
  nil
  "Default extra arguments for the default download type 'Videos'.

Expected value is a list of extra command line arguments for
youtube-dl.")

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


;; ytdl download list
(defvar ytdl--download-list
  (make-hash-table :test 'equal)
  "Hash table of current `ytdl` downloads.

Keys are UUID.
`ytdl--list-entry'.")

(defvar ytdl--dl-list-mode-map
  (let ((map (copy-keymap special-mode-map)))
    (prog1 map
      (define-key map "?" #'ytdl--dispatch)
      (define-key map "g" #'ytdl--refresh-list)
      (define-key map "o" #'ytdl--open-item)
      (define-key map "k" #'ytdl--delete-item-at-point)
      (define-key map "K" #'ytdl--delete-item-and-file-at-point)
      (define-key map "d" #'ytdl--delete-marked-items)
      (define-key map "D" #'ytdl--delete-marked-items-and-files)
      (define-key map "e" #'ytdl--show-error)
      (define-key map "m" #'ytdl--mark)
      (define-key map "m" #'ytdl--mark-all)
      (define-key map "u" #'ytdl--unmark)
      (define-key map "U" #'ytdl--unmark-all)
      (define-key map "y" #'ytdl--copy-item-path)))
  "Keymap for `ytdl--dl-list-mode'.")

(defcustom ytdl-dl-buffer-name
  "*ytdl-list*"
  "Name of `ytdl` download list buffer."
  :type '(string)
  :group 'ytdl)

;; Object storing all data related to a download item in ytdl
(cl-defstruct ytdl--list-entry
  title
  status
  type
  path
  size
  error
  process-id)

(defvar ytdl--marked-items
  '()
  "List of marked items.")

;; Functions
(defun ytdl--message (msg)
  "Diplay MSG starting with `ytdl-message-start'."
  (message (concat ytdl-message-start
                   msg)))

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

Returns (download-type destination-folder extra-args)."

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
                `(,(nth 0 x) ,(nth 2 x) ,(nth 3 x))))
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
                                       (shell-quote-argument filename))))


(defun ytdl--download-async (url filename extra-ydl-args &optional finish-function dl-type)
  "Asynchronously download video at URL into FILENAME.

Extra arguments to ytdl can be provided with EXTRA-YDL-ARGS.

FINISH-FUNCTION is a function that is executed once the file is
downloaded.  It takes a single argument (file-path).

DL-TYPE is the download type, see `ytdl-download-types'."
  (ytdl--eval-mode-line-string 1)
  (let ((process-id)
        (uuid (ytdl--uuid url)))
    (setq process-id
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
                   (setf (ytdl--list-entry-status (gethash uuid
                                                           ytdl--download-list))
                         "error")
                   (setf (ytdl--list-entry-error (gethash uuid
                                                          ytdl--download-list))
                         response)
                   (ytdl--eval-mode-line-string -1)
                   (message (concat ytdl-message-start
                                    response)))
               (ytdl--async-download-finished response uuid)
               (when finish-function
                 (funcall finish-function response)))
             (ytdl--refresh-list))))
    (puthash uuid (make-ytdl--list-entry :title (file-name-nondirectory filename)
                                         :status "downloading"
                                         :type (or dl-type "Unknown")
                                         :path nil
                                         :size "?"
                                         :error nil
                                         :process-id process-id)
             ytdl--download-list))
  (ytdl--refresh-list))


(defun ytdl--async-download-finished (filename uuid)
  "Generic function run after download is completed.

FILENAME is the absolute path of the file downloaded by`ytdl--download-async'.  See `ytdl--download-async' for more
details.

UUID is the key of the list item in `ytdl--download-list'."
  (setq ytdl--last-downloaded-file-name filename)
  (setf (ytdl--list-entry-status (gethash uuid
                                          ytdl--download-list))
        "downloaded")
  (setf (ytdl--list-entry-path (gethash uuid
                                        ytdl--download-list))
        filename)
  (setf (ytdl--list-entry-size (gethash uuid
                                        ytdl--download-list))
        (file-size-human-readable (file-attribute-size
                                   (file-attributes
                                    filename))))
  (ytdl--eval-mode-line-string -1)
  (ytdl--message (concat "Video downloaded: "
                         filename)))


(defun ytdl--get-args ()
  "Query user for ytdl arguments."
  (let* ((url-at-point (thing-at-point 'url t))
         (url (or url-at-point
                  (read-from-minibuffer (concat ytdl-message-start
                                                "URL: ")
                                        (current-kill 0))))
         (dl-type (ytdl--get-download-type))
         (dl-type-name (nth 0 dl-type))
         (destination-folder (ytdl--eval-field (nth 1 dl-type)))
         (filename (ytdl-get-filename  destination-folder url))
         (extra-ydl-args (ytdl--eval-list (ytdl--eval-field (nth 2 dl-type))))
         (run-ytdl? (ytdl--destination-folder-exists-p destination-folder)))
    (list url filename extra-ydl-args run-ytdl? dl-type-name)))


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
         (run-ytdl? (nth 3 out))
         (dl-type-name (nth 4 out)))
    (when run-ytdl?
      (ytdl--download-async url
                            filename
                            extra-ydl-args
                            nil
                            dl-type-name))))


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




;; ytdl download list
(transient-define-prefix ytdl--dispatch ()
  "Invoke a ytdl command from a list of available commands."
  ["ytdl-download-list commands"
   [("g" "refresh" ytdl--refresh-list )
    ("o" "open file in media player" ytdl--open-item)
    ("k" "remove from list" ytdl--delete-item-at-point )
    ("K" "remove from list and delete file" ytdl--delete-item-and-file-at-point )
    ("e" "show eventual error(s)" ytdl--show-error)
    ("y" "copy file path" ytdl--copy-item-path)]
   [("m" "mark item(s) at point" ytdl--mark)
    ("M" "mark all items" ytdl--mark-all)
    ("u" "unmark item(s) at point" ytdl--unmark)
    ("U" "unmark all marked items" ytdl--unmark-all)
    ("d" "remove mark items from list" ytdl--delete-marked-items)
    ("D" "remove mark items from list and delete files" ytdl--delete-marked-items-and-files)]])



(defun ytdl--uuid (url)
  "Generate a UUID using URL.

UUID consist of URL and a time stamp '%Y-%m-%d-%T'."
  (concat url
          (url-encode-url (format-time-string "%Y-%m-%d-%T"))))


(define-derived-mode ytdl--dl-list-mode tabulated-list-mode "ytdl-mode"
  "Major mode for `ytdl' download list."
  (hl-line-mode)
  (use-local-map ytdl--dl-list-mode-map)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-format
        `[("Title" 35 t)
          ("Status" 15 nil)
          ("Size" 10 nil)
          ("Download type" 30 nil)])
  (add-hook 'tabulated-list-revert-hook 'ytdl--refresh-list nil t))


(defun ytdl--format-item-list()
  (let ((item-list '()))
    (maphash (lambda (key item)
               (setq item-list (append item-list
                                       (list (list key
                                                   (ytdl--item-to-vector item))))))
             ytdl--download-list)
    item-list))


(defun ytdl--item-to-vector (item)
  "Convert item into vector for `tabulated-list-mode'."
  (vector (ytdl--list-entry-title  item)
          (ytdl--list-entry-status item)
          (ytdl--list-entry-size item)
          (ytdl--list-entry-type  item)))


(defun ytdl-show-list ()
  (interactive)
  (pop-to-buffer (with-current-buffer (get-buffer-create ytdl-dl-buffer-name)
                   (ytdl--dl-list-mode)
                   (ytdl--refresh-list)
                   (current-buffer))))


(defun ytdl--refresh-list ()
  "Refresh `ytdl-dl-buffer-name'."
  (interactive)
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create ytdl-dl-buffer-name)
      (tabulated-list-init-header)
      (setq tabulated-list-entries (ytdl--format-item-list))
      (tabulated-list-print t)
      (goto-char (point-min))
      (while (tabulated-list-get-id)
        (if (-contains? ytdl--marked-items (tabulated-list-get-id))
            (progn
              (tabulated-list-put-tag "*")
              (forward-line))
          (forward-line))))))


(defun ytdl--get-item-object(&optional key)
  "Get object of item at point.

If KEY is provided then get the object with that key in
`ytdl--download-list'."
  (let ((key (or key
                 (tabulated-list-get-id))))
    (gethash  key
              ytdl--download-list)))


;; list of ytdl download list commands

(defun ytdl--delete-item-from-dl-list (key &optional delete-file? no-confirmation)
  "Delete ITEM from `ytdl--download-list'.

If DELETE-FILE? is non-nil then delete associated file on the
disk. Else delete item from the download list only.

If NO-CONFIRMATION is nil, then ask user for
confirmation. Else perform the operation directly."

  (let* ((item (ytdl--get-item-object key))
         (status (ytdl--list-entry-status item)))
    (when (or no-confirmation
              (y-or-n-p (concat "[ytdl] Are you sure to "
                                (if (string= status "downloading")
                                    "interrupt this download"
                                  "delete this item")
                                "?"
                                (when (and delete-file?
                                           (string= status "downloaded"))
                                  " The associated file will be deleted."))))
      (when (string= status "downloading")
        (interrupt-process (ytdl--list-entry-process-id item))
        (ytdl--eval-mode-line-string -1))
      (when (and (string= status "downloaded")
                 delete-file?)
        (delete-file (ytdl--list-entry-path item)))
      (remhash key
               ytdl--download-list))))


(defun  ytdl--delete-item-at-point ()
  "Delete item(s) at point from list.

If the item at point is still being downloaded, then interrupt
the process.

Note that this function does not delete the eventual file on the
disk.  See `ytdl--delete-item-and-file-at-point' for that feature."
  (interactive)
  (if (region-active-p)
      (let ((count (count-lines
                    (region-beginning)
                    (region-end))))
        (when (y-or-n-p (concat "[ytdl] Are you sure to remove those "
                                (int-to-string count)
                                " items?"))
          (save-mark-and-excursion
            (goto-char (region-beginning))
            (dotimes (_ count)
              (ytdl--delete-item-from-dl-list (tabulated-list-get-id) nil t)
              (forward-line)))))
    (ytdl--delete-item-from-dl-list (tabulated-list-get-id)))
  (ytdl--refresh-list))


(defun ytdl--delete-item-and-file-at-point ()
  "Delete item(s) at point from list and associated file.

If the item at point is still being downloaded, then interrupt
the process."
  (interactive)
  (if (region-active-p)
      (let ((count (count-lines
                    (region-beginning)
                    (region-end))))
        (when (y-or-n-p (concat "[ytdl] Are you sure to remove those "
                                (int-to-string count)
                                " items?"
                                "The associated files will be deleted as well."))
          (save-mark-and-excursion
            (goto-char (region-beginning))
            (dotimes (_ count)
              (ytdl--delete-item-from-dl-list (tabulated-list-get-id) t t)
              (forward-line)))))
    (ytdl--delete-item-from-dl-list (tabulated-list-get-id) t))
  (ytdl--refresh-list))


(defun ytdl--delete-marked-items ()
  "Delete marked item(s) from download list."
  (interactive)
  (if  ytdl--marked-items
      (when (y-or-n-p (concat "[ytdl] Are you sure to remove those "
                              (int-to-string (length ytdl--marked-items))
                              " item(s)?"))
        (dolist (key ytdl--marked-items)
          (ytdl--delete-item-from-dl-list key nil t))
        (setq ytdl--marked-items '())
        (ytdl--refresh-list))
    (ytdl--message "No marked item.")))


(defun ytdl--delete-marked-items-and-files ()
  "Delete marked item(s) from download list and associated file(s)."
  (interactive)
  (if ytdl--marked-items
      (when (y-or-n-p (concat "[ytdl] Are you sure to remove those "
                              (int-to-string (length ytdl--marked-items))
                              " item(s)?"
                              "The associated files will be deleted as well."))
        (dolist (key ytdl--marked-items)
          (ytdl--delete-item-from-dl-list key t t))
        (setq ytdl--marked-items '())
        (ytdl--refresh-list))
    (ytdl--message "No marked item.")))


(defun ytdl--open-item()
  "Open item at point in media player.

                          To configure the media player for `ytdl', see
                          `ytdl-media-player'."
  (interactive)
  (let ((item (ytdl--get-item-object)))
    (if (not (string= (ytdl--list-entry-status item)
                      "downloaded"))
        (ytdl--message "File is not downloaded yet...")
      (ytdl--message "Opening file")
      (ytdl--open-file-in-media-player (ytdl--list-entry-path item)))))


(defun  ytdl--copy-item-path ()
  "Copy path of the higlighted list item to kill ring."
  (interactive)
  (let ((item (ytdl--get-item-object)))
    (if (not (string= (ytdl--list-entry-status item)
                      "downloaded"))
        (ytdl--message "File is not downloaded yet...")
      (let ((path (ytdl--list-entry-path item)))
        (ytdl--message (concat "File path is: " path ". Added to kill-ring."))
        (kill-new path)))))


(defun ytdl--show-error()
  "Show eventual errors for item at point."
  (interactive)
  (let ((item (ytdl--get-item-object)))
    (ytdl--message
     (if (string= (ytdl--list-entry-status item)
                  "error")
         (ytdl--list-entry-error item)
       (concat "Video is "
               (ytdl--list-entry-status item)
               ".")))))


(defun ytdl--mark-at-point (&optional count)
  "Mark item at point"
  (interactive "p")
  (let ((count (or (abs count) 1)))
    (dotimes (_ count)
      (when (tabulated-list-get-id)
        (setq ytdl--marked-items (append ytdl--marked-items (list (tabulated-list-get-id))))
        (tabulated-list-put-tag "*")
        (forward-line)))))

(setq ytdl--marked-items '())

(defun ytdl--unmark-at-point (&optional count)
  "Mark item at point"
  (interactive "p")
  (let ((count (or (abs count) 1)))
    (dotimes (_ count)
      (when (tabulated-list-get-id)
        (message (tabulated-list-get-id))
        (setq ytdl--marked-items (remove (tabulated-list-get-id) ytdl--marked-items))
        (tabulated-list-put-tag "")
        (forward-line)))))

(defun ytdl--mark (&optional count)
  "Mark items.

With numeric argument, mark that many times.
When region is active, mark all entries in region."
  (interactive "p")
  (if (region-active-p)
      (let ((count (count-lines
                    (region-beginning)
                    (region-end))))
        (save-mark-and-excursion
          (goto-char (region-beginning))
          (ytdl--mark-at-point count)))
    (ytdl--mark-at-point count)))

(defun ytdl--unmark (&optional count)
  "Unmark items.

With numeric argument, mark that many times.
When region is active, mark all entries in region."
  (interactive "p")
  (if (region-active-p)
      (let ((count (count-lines
                    (region-beginning)
                    (region-end))))
        (save-mark-and-excursion
          (goto-char (region-beginning))
          (ytdl--unmark-at-point count)))
    (ytdl--unmark-at-point count)))


(defun ytdl--mark-all ()
  "Mmark all marked items."
  (interactive)
  (maphash (lambda (key item)
             (setq ytdl--marked-items (append ytdl--marked-items `(,key))))
           ytdl--download-list)
  (ytdl--refresh-list))

(defun ytdl--unmark-all ()
  "Unmark all marked items."
  (interactive)
  (setq ytdl--marked-items '())
  (ytdl--refresh-list))

(provide 'ytdl)
;;; ytdl.el ends here

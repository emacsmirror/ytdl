;;; ytdl.el --- Emacs Interface for youtube-dl -*- lexical-binding: t -*-

;; Copyright (C) 2020 Arnaud Hoffmann

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


(defgroup ytdl
  nil
  "Emacs interface for ytdl."
  :group 'external)

(defvar ytdl-version
  "1.2.2"
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
      (define-key map "g" #'ytdl--refresh-download-list-buffer)
      (define-key map "o" #'ytdl--open-item)
      (define-key map "k" #'ytdl--delete-item)
      (define-key map "K" #'ytdl--delete-item-and-file)
      (define-key map "y" #'ytdl--copy-item-path)
      ))
  "Keymap for `ytdl--dl-list-mode'.")

(defcustom ytdl-dl-buffer-string
  "%-40s %-15s %-7s %s"
  "String used to format ytdl download list buffer.

This variable should be consistent with
`ytdl-dl-buffer-fields-to-print'."
  :group 'ytdl
  :type '(string))

(defcustom ytdl-dl-buffer-fields-to-print
  '("title" "status" "size" "type")
  "List of fields to be printed in ytdl download list buffer.

The fields should be strings adn selected among the slots of
`ytdl--list-entry'.

This variable should be consistent with
`ytdl-dl-buffer-string'."
  :group 'ytdl
  :type '(string))

(defcustom ytdl--dl-buffer-name
  "*ytdl-list*"
  "Name of `ytdl` download list buffer."
  :type '(string)
  :group 'ytdl)

(defvar ytdl--mapping-list
  nil
  "Internal map used by `ytdl'.

The list maps implicitely the line index of a `ytdl--list-entry'
in `ytdl-dl-list' buffer and its unique ID.  The mapping is done
implicietly through the position of each UID in the
list (i.e. position = line_index - 1).")

;; Object storing all data related to a download item in ytdl
(cl-defstruct ytdl--list-entry
  title
  status
  type
  path
  size
  process-id)


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
  "Asynchronously download URL into FILENAME.

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
                   (ytdl--eval-mode-line-string -1)
                   (message (concat ytdl-message-start
                                    response)))
               (ytdl--async-download-finished response uuid)
               (when finish-function
                 (funcall finish-function response))))))
    (puthash uuid (make-ytdl--list-entry :title (file-name-nondirectory filename)
                                         :status "downloading"
                                         :type (or dl-type "Unknown")
                                         :path nil
                                         :size "?"
                                         :process-id process-id)
             ytdl--download-list)))


(defun ytdl--async-download-finished (filename uuid)
  "Generic function run after download is completed.

FILENAME is the absolute path of the file downloaded by
`ytdl--download-async'.  See `ytdl--download-async' for more
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
                                    "/home/tuedachu/perso/videos/b.mkv"))))
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
  ;; This function was inspired from magit
  ;; see https://magit.vc/ for more details
  ["ytdl-download-list commands"
   [("g" "refresh" ytdl--refresh-download-list-buffer)
    ("o" "open file in media player" ytdl--open-item)
    ("k" "remove from list" ytdl--delete-item)
    ("K" "remove from list and delete file" ytdl--delete-item-and-file )
    ("y" "copy file path" ytdl--copy-item-path)]])


(defun ytdl--uuid (url)
  "Generate a UUID using URL."
  (concat url
          (url-encode-url (time-stamp-string))))

(define-derived-mode ytdl--dl-list-mode
  special-mode "ytdl-mode"
  "Major mode for `ytdl' download list."
  (hl-line-mode)
  (use-local-map ytdl--dl-list-mode-map)
  (setf truncate-lines t
        header-line-format (format ytdl-dl-buffer-string
                                   "Title" "Status" "Size" "Download type")))


(defun ytdl--refresh-download-list-buffer()
  "Refresh ytdl download list buffer.

Read `ytdl--download-list' and print information of each download
list entry in ytdl buffer adn make it current.

For configuration, see `ytdl-dl-buffer-string' and
`ytdl-dl-buffer-fields-to-print'."
  ;; This function is inspired from Skeeto's youtube-dl
  ;; See: https://github.com/skeeto/youtube-dl-emacs
  (interactive)
  (ytdl--message "Refreshing")
  (setq ytdl--mapping-list nil)
  (pop-to-buffer (with-current-buffer (get-buffer-create ytdl--dl-buffer-name)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (maphash (lambda (key item)
                                (setq ytdl--mapping-list (append ytdl--mapping-list
                                                                 `(,key)))
                                (ytdl--print-item item))
                              ytdl--download-list)
                     (ytdl--dl-list-mode)
                     (current-buffer)))))


(defalias 'ytdl-show-list 'ytdl--refresh-download-list-buffer)


(defun ytdl--print-item (item)
  "Print item  information of ITEM in ytdl dl buffer.

See `ytdl-dl-buffer-string' and `ytdl-dl-buffer-fields-to-print'
to configure the layout of ytdl download list buffer."
  (let ((counter 0)
        (format-str (split-string ytdl-dl-buffer-string)))
    (while (< counter
              (length ytdl-dl-buffer-fields-to-print))
      (insert (format (concat (nth counter format-str)
                              " ")
                      (funcall (intern (concat "ytdl--list-entry-"
                                               (nth counter
                                                    ytdl-dl-buffer-fields-to-print)))
                               item)))
      (setq counter (1+ counter)))
    (insert "\n")))


(defun ytdl--get-item-key()
  "Get key of highlighted list item."
  (nth (1- (line-number-at-pos))
       ytdl--mapping-list))


(defun ytdl--get-item-object()
  "Get objevt if highlighted list item."
  (gethash (ytdl--get-item-key)
           ytdl--download-list))


;; list of ytdl download list commands
(defun  ytdl--delete-item ()
  "Delete highlighted list item from list.

If the highlighted item is still being downloaded, then interrupt
the process.

Note that this function does not delete the eventual file on the
disk.  See `ytdl--delete-item-and-file' for that feature."
  (interactive)
  (let ((item (ytdl--get-item-object)))
    (when (string= (ytdl--list-entry-status item)
                   "downloading")
      (interrupt-process (ytdl--list-entry-process-id item))
      (ytdl--eval-mode-line-string -1))
    (remhash (ytdl--get-item-key)
             ytdl--download-list)
    (ytdl--refresh-download-list-buffer)))


(defun ytdl--delete-item-and-file()
  "Delete highlighted list item from list and disk.

If the highlighted item is still being downloaded, then interrupt
the process."
  (interactive)
  (let* ((item (ytdl--get-item-object))
         (status (ytdl--list-entry-status item)))
    (when (string= status "downloading")
      (interrupt-process (ytdl--list-entry-process-id item))
      (ytdl--eval-mode-line-string -1))
    (when (string= status "downloaded")
      (delete-file (ytdl--list-entry-path item)))
    (remhash (ytdl--get-item-key)
             ytdl--download-list)
    (ytdl--refresh-download-list-buffer)))


(defun ytdl--open-item()
  "Open highlighted item in media player.

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


(provide 'ytdl)
;;; ytdl.el ends here

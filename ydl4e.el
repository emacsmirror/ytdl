;;; ydl4e.el --- Emacs Interface for youtube-dl -*- lexical-binding: t -*-

;; Copyright (C) 2019

;; Author: Arnaud Hoffmann <tuedachu@gmail.com>
;; Maintainer: Arnaud Hoffmann <tuedachu@gmail.com>
;; URL:
;; Package-Version:
;; Version:
;; Package-Requires:
;; Keywords:

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
;; * Setup
;;

(require 'eshell)

(defcustom ydl4e-music-folder
  (expand-file-name "~/music")
  "Folder where music will be downloaded"
  :group 'ydl4e
  :type '(string))

(defcustom ydl4e-video-folder
  nil
  "Folder where videos will be downloaded."
  :group 'ydl4e
  :type '(string))

(defcustom ydl4e-download-folder
  (expand-file-name "~/Downloads/test")
  "Default folder for downloads."
  :group 'ydl4e
  :type '(string))

(defcustom ydl4e-video-format
  "mkv"
  "Encoding format for videos."
  :group 'ydl4
  :type '(string))

(defcustom ydl4e-audio-format
  "mp3"
  "Encoding format for audio."
  :group 'ydl4
  :type '(string))

(defvar ydl4e-download-types
  '(("Downloads" "d" ydl4e-download-folder (concat "--recode-video " ydl4e-video-format))
    ("Music"  "m" ydl4e-music-folder (concat "-x --audio-format " ydl4e-audio-format))
    ("Videos" "v"  ydl4e-video-folder (concat "-x --audio-format " ydl4e-audio-format)))
  "List of destination folders.

Each element is a list '(FIELD-NAME SHORTCUT ABSOLUTE-PATH-TO-FOLDER EXTRA-COMMAND-LINE-ARGS)
where:
FIELD-NAME is a string;
SHORTCUT is a string (only one character);
ABSOLUTE-PATH-TO-FOLDER is the absolute path to the given folder;
EXTRA-COMMAND-LINE-ARGS is extra command line arguments for youtube-dl.")


(defun ydl4e-add-field-in-folder-list (field-name keyboard-shortcut path-to-folder extra-args)
  "Add new field in the list of folders `ydl4e-download-types'."
  (add-to-list 'ydl4e-download-types `(,field-name ,keyboard-shortcut ,path-to-folder ,extra-args)))


(defun ydl4e-run-youtube-dl-eshell(url destination-folder filename &optional extra-ydl-args)
  "Run youtube-dl in a new eshell buffer.

URL is the url of the video to download. DESTINATION-FOLDER is
the folder where the video will be downloaded. FILENAME is the
relative path (from DESTINATION-FOLDER) of the output file.

EXTRA-YDL-ARGS is an optional argument.

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
            " --restrict-filenames"
            " -o " filename)
    (when extra-ydl-args
      (insert " " extra-ydl-args))
    (eshell-send-input)
    (windmove-left)))

(defun ydl4e-run-youtube-dl-sync(url absolute-filename &optional extra-ydl-args)
  (let ((error-message (with-temp-buffer
                         (call-process "youtube-dl" nil t nil url "-o" absolute-filename)
                         (buffer-string))))
    (when (string-match-p "ERROR" error-message)
      error-message)))

(defun ydl4e-get-default-filename (url)
  (with-temp-buffer
    (call-process "youtube-dl" nil '(t nil) nil url "--get-filename" "--restrict-filenames")
    (beginning-of-buffer)
    (search-forward ".")
    (buffer-substring-no-properties (line-beginning-position)
                                    (1- (point)))))

(defun ydl4e-get-download-type()
  "Query download type in mini-buffer.

User can choose candidates from the elements of
`ydl4e-download-types' whose ABSOLUTE-PATH-TO-FOLDER is not nil.

Returns (destination-folder extra-args)."

  (let ((user-input (read-char-choice (concat (propertize "Destination folder:" 'face 'default)
                                              (mapconcat (lambda(x)
                                                           (when (nth 2 x)
                                                             (let* ((destination (nth 0 x))
                                                                    (letter-shortcut (nth 1 x)))
                                                               (concat (propertize " " 'face 'default)
                                                                       (propertize destination 'face 'default)
                                                                       (propertize " [" 'face 'default)
                                                                       (propertize letter-shortcut 'face 'font-lock-warning-face)
                                                                       (propertize "] " 'face 'default)
                                                                       ))))
                                                         ydl4e-download-types
                                                         ""))
                                      (mapcar (lambda(x)
                                                (when (nth 2 x)
                                                  (let* ((destination (nth 0 x)))
                                                    (aref (nth 1 x) 0))))
                                              ydl4e-download-types))))
    (mapcan (lambda(x)
              (when (= (aref (nth 1 x) 0) user-input)
                `(,(nth 2 x) ,(nth 3 x))))
            ydl4e-download-types)))


(defun ydl4e-download (&optional url destination-folder extra-ydl-args)
  (interactive)
  (let* ((url (or url
                  (current-kill 0)))
         (dl-type (ydl4e-get-download-type))
         (destination-folder (or destination-folder
                                 (if (symbolp (nth 0 dl-type))
                                     (symbol-value (nth 0 dl-type))
                                   (nth 0 dl-type))))
         (extra-ydl-args (or extra-ydl-args
                             (eval (nth 1 dl-type))))
         (default-filename (ydl4e-get-default-filename url))
         (filename (read-from-minibuffer "Filename [Default]: " default-filename)))
    (ydl4e-run-youtube-dl-eshell url destination-folder filename extra-ydl-args)
    (minibuffer-message (concat "Video will be downloaded:" (concat destination-folder "/" filename)))))

(provide 'ydl4e)
;;;ydl4e.el ends here

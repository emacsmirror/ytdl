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

(provide 'ydl4e)
;;;ydl4e.el ends here

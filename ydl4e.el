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


(provide 'ydl4e)
;;;ydl4e.el ends here

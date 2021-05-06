# ytdl: An Emacs interface for youtube-dl

[![language](https://img.shields.io/badge/language-elisp-green.svg)](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
[![version](https://img.shields.io/badge/version-1.3.6-green.svg)]()
[![MELPA](https://melpa.org/packages/ytdl-badge.svg)](https://melpa.org/#/ytdl)


`ytdl` is an Emacs-based interface for `youtube-dl`, written in
emacs-lisp.

`youtube-dl` is a command-line program to download videos from
YouTube.com and a few more sites. More information
[here](https://https://youtube-dl.org/).

## Motivation

Command-line tools can be challenging to use (many arguments, need to
use the "--help" command or the man page before actually using the
tool, etc.). Those challenges (or leaning curves) can prevent many
potential users from using the tool.

The idea of `ytdl` is to provide an intuitive and flexible Emacs
interface for `youtube-dl`. `ytdl` leads to considerable gains in
efficiency and comfort, especially when used with `exwm`.

## Usage

Add the following line to your `init.el`:
```elisp
(require 'ytdl)
```

Six interactive functions are available:
- `ytdl-download-eshell`, `ytdl-download` and `ytdl-download-open`
  that can be used to download a media file from an url.
- `ytdl-download-playlist` to download a playlist.
- `ytdl-open-last-downloaded-file` to open the last file downloaded
  with `ytdl` in the configured media player.
- `ytdl-show-list` to show the dowload list.

Note that `ytdl-download`, `ytdl-download-open` and
`ytdl-download-playlist` add the file in the `ytdl` download list that
can be visualized with `ytdl-show-list`. `ytdl-download-eshell` does
not.

### URL

`ytdl` pre-fills the mini-buffer with the URL at point if thing at
point is an URL or the first entry in the kill ring. Users have to
confim by pressing Enter.


### Download List

When calling `ytdl-show-list`, `ytdl` dowload list is shown in a new
buffer. This list contains information about files already downloaded
and files currently being downloaded.

Several actions can be carried out within the download list buffer:


- `?`: Show the help menu with all the commands.
- `g`: Refresh the list
- `o`: Open the file in media player
- `O`: Open all marked files in media player
- `k`: Remove item(s) at point from the list (the file will not be
  affected)
- `K`: Remove item(s) at point from the list and delete the associated
  file from the disk
- `y`: Copy path to file into clipboard
- `m`: mMrk item(s) at point
- `M`: Mark all items
- `u`: Unmark item(s) at point
- `U`: Unmark all items
- `^`: Mark items matching a regexp (to be entered in the mini-buffer)
- `e`: Show eventual error(s)
- `r`: Relaunch item at point
- `R`: Relaunch all items with error(s)
- `d`: remove mark item(s)
- `D`: Remove mark item(s) and associated files
- `c`: Clear downloaded items from list
- `C`: Clear list


### Demo

#### Download a video
![ytdl usage](doc/usage.gif)

#### Download a playlist

![ytdl usage](doc/playlist.gif)


## Inspiration from Skeeto's `youtube-dl.el`

Skeeto's interface was a source of inspiration for the development of
the download list of `ytdl`. His original source code can be found
[here](https://github.com/skeeto/youtube-dl-emacs).


## Configuration

### Download Types

By default, there are three download types:
1. Downloads `ytdl-download-folder` can be customized to change the
destination folder. By default it is set to `"~/Downloads"`. No extra
arguments are given by default, see `ytdl-download-extra-args`.

2. Videos
`ytdl-video-folder` can be customized to change the destination
folder. By default it is set to `nil`. No extra arguments are given by
default, see `ytdl-video-extra-args`.

3. Music `ytdl-music-folder` can be customized to change the
destination folder. By default it is set to `"nil`. By default, the
extra arguments used for this download type are: `'("-x"
"--audio-format" "mp3")`, meaning that audio will be extracted from
the media file and eventually converted into mp3.

To add a new download type, use
`ytdl-add-field-in-download-type-list`. This function takes four
arguments:
- `field-name`: the name displayed in the mini-buffer;
- `keyboard-shortcut`: keyboard shortcut to select this download type
in the mini-buffer;
- `path-to-folder`: absolute path to destination folder;
- `extra-args`: eventual extra arguments to ytdl for this
download type.

#### Example: Add a podcast download type

To add a new download type called "podcasts", add this to the
configuration file:

```elisp
(ytdl-add-field-in-download-type-list "podcasts"
                                       "p"
                                       (expand-file-name "~/podcasts")
                                       nil)
```

### Additional Customization

- By default, `ytdl` does not query the default filename fom the web
  server.

This behavior can be changed by changing `ytdl-always-query-default-filename` to:
- 'never (default behavior)
- 'yes-confirm: query default filenme from teh web server and ask confirmation
- 'yes: query default filenme from teh web server and use it without confirmation.

```elisp
(setq ytdl-always-query-default-filename nil)
```

- Set the media player (used by `ytdl-download-open`) by
  changing the variable `ytdl-media-player`. Default value is
  `mpv`.

- Change the beginning of `ytdl` mini-buffer messages by changing
  `ytdl-message-start`. Default value is `[ytdl]`.

- Hide the `ytdl` information in the global mode line by setting
  `ytdl-mode-line` to `nil`.

- By default, `ytdl` queries the download type using
  `read-char-choice`. If the list of download types is longer than a
  certain value (`ytdl-max-mini-buffer-download-type-entries`), then
  the download type is queried thtough `completing-read` (enabling
  users to use `helm`). To always use `completing-read`, set
  `ytdl-max-mini-buffer-download-type-entries` to 0.

- Change `ytdl` download list buffer name with
  `ytdl-dl-buffer-name`. Default value is `*ytdl-list*`.

- Change the item title column width with
  `ytdl-title-column-width`. Default value is 35.



### Example of configuration

Here is an example of configuration you can add to your `init.el`:

```elisp
(require 'ytdl)

(setq ytdl-music-folder (expand-file-name "~/music")
      ytdl-video-folder (expand-file-name "~/videos")
      ytdl-always-query-default-filename 'never)

(ytdl-add-field-in-download-type-list "podcasts"
                                       "p"
                                       (expand-file-name "~/podcasts")
                                       nil)
```

## Changelog

### v.1.3.6


**FEATURE**:
- Add ytdl-download-finished-hook defcustom. (by Damien Cassou)

**BUG**:
- Show `ytdl-list` when launching a new download. (by Arnaud Hoffmann)
- Remove `emulations` from keyword list. (by Arnaud Hoffmann)

### v.1.3.5

**FEATURE**:
- Add `ytdl-command` defcustom. (by Pierre Neidhardt)

**INTERNAL**:
- Replace ytdl-message-start by `ytdl--concat`. (by Pierre Neidhardt)

**STYLE**:
- Use `error` instead of `(concat ... "ERROR:" ...)`. (by Pierre Neidhardt)
- Fix syntax and docstring typos. (by Pierre Neidhardt)












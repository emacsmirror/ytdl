# ytdl: An Emacs interface for youtube-dl

[![language](https://img.shields.io/badge/language-elisp-green.svg)](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
[![version](https://img.shields.io/badge/version-1.2.2-green.svg)]()

`ytdl` is an Emacs-based interface for `youtube-dl`, written in
emacs-lisp.

`youtube-dl` is a command-line program to download videos from
YouTube.com and a few more sites. More information,
[here](https://github.com/ytdl-org/youtube-dl/blob/master/README.md#readme).

## Motivation

Command-line tools can be challenging to use (many arguments, need to
use the "--help" command or the man page before actually using the
tool, etc.). Those challenges (or leaning curves) can prevent many
potential users from using the tool.

The idea of `ytdl` is to provide an intuitive and flexible Emacs
interface for `youtube-dl`. `ytdl` leads to considerable gains in
efficiency and comfort, especially when used with `exwm` and a
programmable web browser (see [Next
Browser](https://github.com/atlas-engineer/next)).

## Usage

Add the following line to your `init.el`:
```elisp
(require 'ytdl)
```

Five interactive functions are available:
- `ytdl-download-eshell`, `ytdl-download` and `ytdl-download-open`
  that can be used to download a media file from an url.
- `ytdl-open-last-downloaded-file` to open the last file downloaded
  with `ytdl` in the configured media player.
- `ytdl-delete-last-downloaded-file` to delete the last file
  downloaded by `ytdl`.


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

- By default, `ytdl.el` will query the default filename. This operation
can take a few seconds (depending on the web server).

To omit this query, add in your `init.el`:

```elisp
(setq ytdl-always-query-default-filename nil)
```

- To avoid confirmation message when using
`ytdl-delete-last-downloaded-file`, set the value of
`ytdl-always-ask-delete-confirmation` to nil

- Set the media player (used by `ytdl-download-open`) by
  changing the variable `ytdl-media-player`. Default value is
  `mpv`.

- You can change the beginning of the mini-buffer message by changing
  `ytdl-message-start`. Default value is `[ytdl]`.

- You can hide the `ytdl` information in the global mode line by
  setting `ytdl-mode-line` to `nil`.

- By default, `ytdl.el` queries the download type using
  `read-char-choice`. If the list of download types is longer than a
  certain value (`ytdl-max-mini-buffer-download-type-entries`),
  then the download type is queried thtough `completing-read`
  (enabling users to use `helm`). To always use `completing-read`, set
  `ytdl-max-mini-buffer-download-type-entries` to 0.

### Example of configuration

Here is an example of configuration you can add to your `init.el`:

```elisp
(require 'ytdl)

(setq ytdl-music-folder (expand-file-name "~/music")
      ytdl-video-folder (expand-file-name "~/videos")
      ytdl-always-query-default-filename nil
      ytdl-always-ask-delete-confirmation t)

(ytdl-add-field-in-download-type-list "podcasts"
                                       "p"
                                       (expand-file-name "~/podcasts")
                                       nil)
```













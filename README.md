# ydl4e: An Emacs interface for youtube-dl

[![language](https://img.shields.io/badge/language-elisp-green.svg)](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
[![version](https://img.shields.io/badge/version-1.2.0-green.svg)]()

`ydl4e` is an Emacs-based interface for youtube-dl, written in
emacs-lisp.

youtube-dl is a command-line program to download videos from
YouTube.com and a few more sites. More information,
[here](https://github.com/ytdl-org/youtube-dl/blob/master/README.md#readme).

## Motivation

Command-line tools can be challenging to use (many arguments, need to
use the "--help" command or the man page before actually using the
tool, etc.). Those challenges (or leaning curves) can prevent many
potential users from using the tool.

The idea of `ydl4e` is to provide an intuitive and flexible Emacs
interface for youtube-dl. `ydl4e` leads to considerable gains in
efficiency and comfort, especially when used with `exwm` and a
programmable web browser (see [Next
Browser](https://next.atlas.engineer)).

## Usage

Add the following line to your `init.el`:
```elisp
(require 'ydl4e)
```

Five interactive functions are available:
- `ydl4e-download-eshell`, `ydl4e-download` and `ydl4e-download-open`
  that can be used to download a media file from an url.
- `ydl4e-open-last-downloaded-file` to open the last file downloaded
  with `ydl4e` in the configured media player.
- `ydl4e-delete-last-downloaded-file` to delete the last file
  downloaded by `ydl4e`.


## Configuration

### Download Types

By default, there are three download types:
1. Downloads `ydl4e-download-folder` can be customized to change the
destination folder. By default it is set to `"~/Downloads"`. No extra
arguments are given by default, see `ydl4e-download-extra-args`.

2. Videos
`ydl4e-video-folder` can be customized to change the destination
folder. By default it is set to `nil`. No extra arguments are given by
default, see `ydl4e-video-extra-args`.

3. Music `ydl4e-music-folder` can be customized to change the
destination folder. By default it is set to `"nil`. By default, the
extra arguments used for this download type are: `'("-x"
"--audio-format" "mp3")`, meaning that audio will be extracted from
the media file and eventually converted into mp3.

To add a new download type, use
`ydl4e-add-field-in-download-type-list`. This function takes four
arguments:
- `field-name`: the name displayed in the mini-buffer;
- `keyboard-shortcut`: keyboard shortcut to select this download type
in the mini-buffer;
- `path-to-folder`: absolute path to destination folder;
- `extra-args`: eventual extra arguments to youtube-dl for this
download type.

#### Example: Add a podcast download type

To add a new download type called "podcasts", add this to the
configuration file:

```elisp
(ydl4e-add-field-in-download-type-list "podcasts"
                                       "p"
                                       (expand-file-name "~/podcasts")
                                       nil)
```

### Additional Customization

- By default, `ydl4e` will query the default filename. This operation
can take a few seconds (depending on the web server).

To omit this query, add in your `init.el`:

```elisp
(setq ydl4e-always-query-default-filename nil)
```

- You can set the value of `ydl4e-always-ask-delete-confirmation` to
nil to avoid confirmation message when using
`ydl4e-delete-last-downloaded-file`.

- You can set up your media player (used by `ydl4e-download-open`) by
  changing the variable `ydl4e-media-player`. Default value is `mpv`.

- You can change the beginning of the mini-buffer message by changing
  `ydl4e-message-start`. Default value is `[ydl4e]`.

- You can hide the `ydl4e` information in the global mode line by
  setting `ydl4e-mode-line` to `nil`.

### Example of configuration

Here is an example of configuration you can add to your `init.el`:

```elisp
(require 'ydl4e)

(setq ydl4e-music-folder (expand-file-name "~/music")
      ydl4e-video-folder (expand-file-name "~/videos")
      ydl4e-always-query-default-filename nil
      ydl4e-always-ask-delete-confirmation t)

(ydl4e-add-field-in-download-type-list "podcasts"
                                       "p"
                                       (expand-file-name "~/podcasts")
                                       nil)
```











# youtube-dl.el: An Emacs interface for youtube-dl

[![language](https://img.shields.io/badge/language-elisp-green.svg)](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
[![version](https://img.shields.io/badge/version-1.2.1-green.svg)]()

`youtube-dl.el` is an Emacs-based interface for youtube-dl, written in
emacs-lisp.

youtube-dl is a command-line program to download videos from
YouTube.com and a few more sites. More information,
[here](https://github.com/ytdl-org/youtube-dl/blob/master/README.md#readme).

## Motivation

Command-line tools can be challenging to use (many arguments, need to
use the "--help" command or the man page before actually using the
tool, etc.). Those challenges (or leaning curves) can prevent many
potential users from using the tool.

The idea of `youtube-dl.el` is to provide an intuitive and flexible Emacs
interface for youtube-dl. `youtube-dl.el` leads to considerable gains in
efficiency and comfort, especially when used with `exwm` and a
programmable web browser (see [Next
Browser](https://next.atlas.engineer)).

## Usage

Add the following line to your `init.el`:
```elisp
(require 'youtube-dl)
```

Five interactive functions are available:
- `youtube-dl-download-eshell`, `youtube-dl-download` and `youtube-dl-download-open`
  that can be used to download a media file from an url.
- `youtube-dl-open-last-downloaded-file` to open the last file downloaded
  with `youtube-dl` in the configured media player.
- `youtube-dl-delete-last-downloaded-file` to delete the last file
  downloaded by `youtube-dl`.


## Configuration

### Download Types

By default, there are three download types:
1. Downloads `youtube-dl-download-folder` can be customized to change the
destination folder. By default it is set to `"~/Downloads"`. No extra
arguments are given by default, see `youtube-dl-download-extra-args`.

2. Videos
`youtube-dl-video-folder` can be customized to change the destination
folder. By default it is set to `nil`. No extra arguments are given by
default, see `youtube-dl-video-extra-args`.

3. Music `youtube-dl-music-folder` can be customized to change the
destination folder. By default it is set to `"nil`. By default, the
extra arguments used for this download type are: `'("-x"
"--audio-format" "mp3")`, meaning that audio will be extracted from
the media file and eventually converted into mp3.

To add a new download type, use
`youtube-dl-add-field-in-download-type-list`. This function takes four
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
(youtube-dl-add-field-in-download-type-list "podcasts"
                                       "p"
                                       (expand-file-name "~/podcasts")
                                       nil)
```

### Additional Customization

- By default, `youtube-dl` will query the default filename. This operation
can take a few seconds (depending on the web server).

To omit this query, add in your `init.el`:

```elisp
(setq youtube-dl-always-query-default-filename nil)
```

- You can set the value of `youtube-dl-always-ask-delete-confirmation` to
nil to avoid confirmation message when using
`youtube-dl-delete-last-downloaded-file`.

- You can set up your media player (used by `youtube-dl-download-open`) by
  changing the variable `youtube-dl-media-player`. Default value is `mpv`.

- You can change the beginning of the mini-buffer message by changing
  `youtube-dl-message-start`. Default value is `[youtube-dl]`.

- You can hide the `youtube-dl` information in the global mode line by
  setting `youtube-dl-mode-line` to `nil`.

### Example of configuration

Here is an example of configuration you can add to your `init.el`:

```elisp
(require 'youtube-dl)

(setq youtube-dl-music-folder (expand-file-name "~/music")
      youtube-dl-video-folder (expand-file-name "~/videos")
      youtube-dl-always-query-default-filename nil
      youtube-dl-always-ask-delete-confirmation t)

(youtube-dl-add-field-in-download-type-list "podcasts"
                                       "p"
                                       (expand-file-name "~/podcasts")
                                       nil)
```











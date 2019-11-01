# ydl4e: An Emacs interface for youtube-dl

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

The idea of `ydl4e` is to provide an intuive and flexible Emacs
interface for youtube-dl. `ydl4e` leads to considerable gains in
efficiency and confomrt, expecially when used with `exwm` and a
programmable web browser (see [Next
Browser](https://next.atlas.engineer)).

## Usage

Add the following line to your `init.el`:
```elisp
(require 'ydl4e)
```

Two interactive functions are available: `ydl4e-download` and
`ydl4e-download-open`.

- `ydl4e-download`: Two ways to use this function interactively:
1. `M-x ydl4e-download`: Download the file at the url stored in the
kill-ring.
2. `M-x ydl4e-download https://a-website.domain`: Download the file at
the provided url.

- The same applies for the other function `ydl4e-download-open`.


## Configuration

### Download Types

By default, there are three download types:
1. Downloads
- `ydl4e-download-folder` can be customized to change the destination
folder. By default it is set to `"~/Downloads"`.
- The format of the downloaded file is controled by
`ydl4e-download-format`. By default, it is set to `"mkv"`.

2. Videos
- `ydl4e-video-folder` can be customized to change the destination
folder. By default it is set to `nil`.
- The format of the downloaded file is controled by
`ydl4e-video-format`. By default, it is set to `"mkv"`.

3. Music
- `ydl4e-music-folder` can be customized to change the destination
folder. By default it is set to `"nil`.
- The format of the downloaded file is controled by
`ydl4e-audio-format`. By default, it is set to `"mp3"`.

To add a new download type, use
`ydl4e-add-field-in-download-type-list`. This function takes four
arguments:
- `field-name`: the name displayed in the mini-buffer;
- `keyboard-shortcut`: keyboard shortcut to select this download type
in the minibuffer;
- `path-to-folder`: absolute path to destination folder;
- `extra-args`: eventual extra arguments to youtube-dl for this
download type.

#### Example: Add a podcast download type

To add a new download type callde "podcasts", add this to the
configuration file:

```elisp
(ydl4e-add-field-in-download-type-list "Podcasts"
"p"
(expand-file-name "~/podcasts")
'("x" "--audio-format" "mp3"))
```

### Additional Customization

- By default, functions `ydl4e-download-open` and `ydl4e-download`
will query the default filename. This operation can take a few
seconds (depending on the web server).

To omit this query, add in your `init.el`:

```elisp
(setq ydl4e-always-query-default-filename nil)
```

- You can set the value of `ydl4e-always-ask-delete-confirmation` to
nil to avoid confirmation message when using
`ydl4e-delete-last-downloaded-file`.








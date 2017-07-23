# followcursor-mode - highlight line in other buffer containing word in current buffer

*Author:* T.v.Dein <tlinden@cpan.org><br>
*URL:* [https://github.com/tlinden/followcursor](https://github.com/tlinden/followcursor)<br>

## Screenshot:

![demo](imgs/followcursor-mode-screencast.gif | =800x500)

## Usage:

To use, first add to your config:

        (require 'followcursor-mode

Then prepare two windows with different buffers, enable the mode
in one of them with:

        (followcursor-mode)

Move around in that buffer as you wish. Whenever `point` is on a
match, it will be highlighted in both buffers.




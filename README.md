# elogcat.el

logcat interface for emacs based on android-mode.el

## ScreenShot


## Installation

It's available on [Melpa](https://melpa.org/):

    M-x package-install elogcat

Requirements

- **ADB**
[Installing the Android SDK](https://developer.android.com/sdk/installing/)

You can add these lines to your init file.

```elisp
(require 'elogcat)
```


Key bindings

Key | Function
--- | --------
 C | elogcat-erase-buffer
 i | elogcat-set-include-filter
 x | elogcat-set-exclude-filter
 I | elogcat-clear-include-filter
 X | elogcat-clear-exclude-filter
 g | elogcat-show-status
 F | occur
 q | elogcat-delete-window
 m | elogcat-toggle-main
 s | elogcat-toggle-system
 e | elogcat-toggle-events
 r | elogcat-toggle-radio
 k | elogcat-toggle-kernel

## License

Copyright (C) 2015 Youngjoo Lee

Author: Youngjoo Lee <youngker@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

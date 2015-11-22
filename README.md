# elogcat.el [![MELPA](http://melpa.org/packages/elogcat-badge.svg)](http://melpa.org/#/elogcat)

logcat interface for emacs based on [android-mode](https://github.com/remvee/android-mode)

## ScreenShot

- **elogcat**
<img align="center" src="https://raw.github.com/youngker/elogcat.el/master/elogcat.png">

## Installation

It's available on [Melpa](https://melpa.org/):

    M-x package-install elogcat

Requirements

- **adb**

[Installing the Android SDK](https://developer.android.com/sdk/installing/)

You can add these lines to your init file.

```elisp
(require 'elogcat)
```


Key bindings

Key | Function
--- | --------
<kbd>C</kbd> | elogcat-erase-buffer
<kbd>i</kbd> | elogcat-set-include-filter
<kbd>x</kbd> | elogcat-set-exclude-filter
<kbd>I</kbd> | elogcat-clear-include-filter
<kbd>X</kbd> | elogcat-clear-exclude-filter
<kbd>g</kbd> | elogcat-show-status
<kbd>F</kbd> | occur
<kbd>m</kbd> | elogcat-toggle-main
<kbd>s</kbd> | elogcat-toggle-system
<kbd>e</kbd> | elogcat-toggle-events
<kbd>r</kbd> | elogcat-toggle-radio
<kbd>k</kbd> | elogcat-toggle-kernel

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

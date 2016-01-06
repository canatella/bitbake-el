# bitbake.el - Bitbake integration with emacs

This package provides integration of the Yocto Bitbake tool with
emacs. Its main features are:

* interacting with the bitbake script so that you can run bitbake
  seamlessly from emacs. If your editing a recipe, recompiling is just
  one `M-x bitbake-recompile` command away,

* deploying recipes output directly to your target device over ssh for
  direct testing (if your image supports read-write mode),

* generating wic images,

* a global minor mode providing menu and shortcuts,

* an mmm based mode to edit bitbake recipes.

## Installing

This package is available on Melpa:
[![MELPA](http://melpa.org/packages/bitbake-badge.svg)](http://melpa.org/#/bitbake)

## Usage

### Calling bitbake from emacs

First you'll need to start bitbake as a server. Use `M-x
bitbake-start-server` to do so. If you haven't setup the
`bitbake-poky-directory` and `bitbake-build-directory` variables, emacs will
prompt for your poky and build directory. You can permanently set them
by running `M-x customize-group bitbake`.

Now that the server is started, you can execute tasks for the current
recipe or any recipe by running `M-x bitbake-task`. There are shortcut for
the most common task, see the BitBake menu in Bitbake minor mode.

### Deploying recipes

If your target device supports ssh and writing to the root file system,
you can use `M-x bitbake-deploy` to deploy a recipe on the device. It will
use tar to create an archive of the files generated on the host and
deploy them on the device. You should make sure that the recipe has
already been build before or use the `M-x bitbake-recompile-deploy`
command.

### Generating wic images

If you use wic to create images, you can use `M-x bitbake-wic-create` to
generate an image. It will prompt for the definition file the image
name. You need to first build the Yocto image or use the command
`M-x bitbake-hdd-image` which will first run `bitbake image` and then wic.
If you set the custom variable `bitbake-flash-device` you'll be able
rebuild and put your wic image directly on the device with the command
`M-x bitbake-flash-image`.

### Bitbake global minor mode

There is also a minor mode which provides shortcut and menu for those
commands, run `M-x bitbake-minor-mode` and look at the BitBake menu
for commands and key strokes.

### Bitbake recipe mode

Finally, you should get syntax highlighting when editing bitbake
recipe files. It uses `mmm-mode` to provide python and shell support
in tasks.

## Development

Development is done on
[GitHub](https://github.com/canatella/bitbake-el). Any bugs or patches
are welcome.

## Contributors

Thanks to

- Lee Yen-Chin for helping me updating the code to support Melpa and
  publishing it,
- Syohei Yoshida for small fixes.

## License

This emacs extension is distributed under the terms of the GPLv3. See
COPYING file for details.

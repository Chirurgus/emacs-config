# Emacs config

My Emacs configuration.

# Setup

The setup is always a kind of lengthy process. First you need to install Emacs
(instant, pre-compiled), then Doom's dependencies (same), and then Doom itself
(about 20 minutes).

You also need to setup the enviromental variables to be just right : 

* Set `HOME` to `C:/Users/You`
* Add `~/.config/emacs/bin/` to `PATH`
* Add Emacs's `bin` folder to `PATH`


Then you can clone the this configuration folder into the `doom` folder :
`git clone git@github.com:Chirurgus/emacs-config.git ~/.config/doom`.

At this point you need to install any packages from this configuration. The're
aren't many but you sould be able to find them by searching for `use-package`
command in `config.el`. Also, you need to donwnload and install the
fonts/icons. First download the font file uisng `:nerd-icons-install-fonts`
from inside Doom, then install the file saved. This is also a good time to
download and install the LanguageTool server (see `use-package lsp-ltex`, in
`config.el`).

Now, ingest the configuration using `doom sync`. Finally, run `doom doctor` to
see if you get any suggestions. Also make sure that you delete `.config` folder
from other possible location : `C:\Users\You\AppData\Roaming\.emacs.d`.

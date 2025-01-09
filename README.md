# `dotfiles`

[![Build Status](https://jenkins.sudo.is/buildStatus/icon?job=ben%2Fdotfiles%2Fmain&style=flat-square)](https://jenkins.sudo.is/job/ben/job/dotfiles/)
![version](https://jenkins.sudo.is/buildStatus/icon?job=ben%2Fdotfiles%2Fmain&style=flat-square&status=${description}&subject=version&build=lastStable&color=blue)
[![git](https://www.sudo.is/readmes/git.sudo.is-ben-dotfiles.svg)](https://git.sudo.is/ben/dotfiles)
[![github](https://www.sudo.is/readmes/github-benediktkr.svg)](https://github.com/benediktkr/dotfiles)
[![codeberg](https://www.sudo.is/readmes/codeberg.svg)](https://codeberg.org/benk/dotfiles)
[![BSD-2-Clause](https://www.sudo.is/readmes/license-BSD-blue.svg)](LICENSE)
[![docs](https://www.sudo.is/readmes/docs.svg)](https://www.sudo.is/docs)
[![matrix](https://www.sudo.is/readmes/matrix-ben-sudo.is.svg)](https://matrix.to/#/@ben:sudo.is)

Here are Ben's dotfiles.

![Leaf Dark](.local/share/backgrounds/Leaf_Dark.png)

## Overview

The dotfiles are currently managed by [`yadm`](https://yadm.io). Files
prior to switching to `yadm` are preserved in the branch `dotfiles`.

## 🐣 Chicken and the egg

Normally `yadm` is executed from `~/.local/bin/yadm`, which is managed
by this repo.

So the first time cloning the repo on a new system, we have a chicken and
egg problem. Either grab `yadm` and and them delete it:

```bash
mkdir -p ~/tmp
wget https://github.com/yadm-dev/yadm/raw/master/yadm -O ~/tmp/yadm
chmod +x ~/tmp/yadm
~/tmp/yadm clone https://git.sudo.is/ben/dotfiles
rm ~/tmp/yadm
```

Or use the [`clone.sh` script](https://git.sudo.is/ben/dotfiles/raw/branch/main/.local/share/yadm/clone.sh).

## Repository


- :gitea: **Upstream**: [`git.sudo.is/ben/dotfiles`](https://git.sudo.is/ben/dotfiles)
- :codeberg: **Codeberg**: [`benk/dotfiles`](https://codeberg.org/benk/dotfiles)
- :github: **GitHub**: [`benediktkr/dotfiles`](https://github.com/benediktkr/dotfiles)
- :git: Bitbucket (private)

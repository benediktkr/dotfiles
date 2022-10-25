# KDE

documentation of various things i do with KDE or Plasma

## clean and read-only desktop

create empty dir and make it unwritable

```shell
mkdir ~/.empty
chattr -fR +i ~/.empty
```

optionally set desktop to use that (should use XDG values). for
plasma, this is in `/.config/plasma-org.kde.plasma.desktop-appletsrc`:

```
url=/home/ben/.empty/
```

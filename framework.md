

# grub and kernel modules


 * for longer sleep life: enable `nvme.noacpi`
 * brightness keys clash with ASL. blacklist `hid_sensor_hub` to get control
 * force disable PSR (suttering): `i915.enable_psr=0`

add the options to `GRUB_CMDLINE` in `/etc/default/grub`:

```bash
GRUB_CMDLINE_LINUX_DEFAULT="quiet splash module_blacklist=hid_sensor_hub nvme.noacpi=1 i915.enable_psr=0"
GRUB_CMDLINE_LINUX=""
```

and then update grub

```shell
sudo update-grub
```

# tune power usage

get `powertop`.

```shell
sudo apt-get install powertop

sudo powertop --auto-tune
```

enable as service:

```shell
sudo systemctl enable powertop
sudo systemctl start powertop

# or
sudo tlp start
```

enable ASPM in `/etc/tlp.conf`:

```bash
PCIE_ASPM_ON_BAT=powersupersave
```

start `tlp` and enable it:

```shell
sudo systemctl enable tlp
sudo systemctl start tlp
```

simpler alternative to `tlp` (but they clash):
`power-profiles-daemon`. available in ubuntu repos.

## usb expansion slots

HDMI and SD card appear to use ~1W at idle.

# stuttering/mini-freezes

on kernels pre 5.14, PSR can cause stuttering.

this only seems to happen with wayland from what i have read, and not
X.org. so trying X.org might be worthwhile.

```shell
$ uname -r
5.15.0-48-generic

$ cat /etc/lsb-release | grep DESCR
DISTRIB_DESCRIPTION="Ubuntu 22.04.1 LTS"

$ sudo cat /sys/kernel/debug/dri/0/i915_edp_psr_status
Sink support: yes [0x03]
PSR mode: PSR1 enabled
Source PSR ctl: enabled [0x81f006e6]
Source PSR status: SRDENT [0x40010000]
Busy frontbuffer bits: 0x00000000

```

but lets try that anyway.

add `i915.enable_psr=0` to `GRUB_CMDLINE` in `/etc/default/grub`.

```bash
GRUB_CMDLINE_LINUX_DEFAULT="quiet splash i915.enable_psr=0"
```

or create the file `/etc/modprobe.d/i915.conf`:

```
options i915 enable_psr=0
```

and update grub:

```shell
sudo update-grub
```

## fiddling with it directly and checking values

install `systool`:

```shell
sudo apt-get install sysfsutils
```

check what it tells us:

```shell
$ sudo systool -v -m i915 | grep psr
    enable_psr2_sel_fetch= "N"
    enable_psr          = "-1"
    psr_safest_params   = "N"
```



```shell

# list all options for the i915 module (drop the grep to see all)
sudo modprobe --showconfig i915 | grep psr


sudo modprobe i915 enable_psr=0
```


# firefox

enable gpu rendering, install `intel-gpu-tools` and possibly
`intel-media-driver` (not available on Ubuntu 22.04 repos).

```shell
sudo apt-get install intel-gpu-tools
```

in firefox, open `about:config` and set:

 * `media.rdd-ffmpeg.enabled`: `true`
 * `media.ffmpeg.vaapi.enabled`: `true`
 * `media.navigator.mediadatadecoder_vpx_enabled`: `true`
 * `media.ffvpx.enabled`: `false` (created new)
 * `media.rdd-vpx.enabled`: `false`
 * `media.rdd-process.enabled`: falsee

# check gpu power usage

check gpu power usage with `intel_gpu_top`:

```shell

sudo intel_gpu_top
```

play a 4K video or something to to get some load

# forum posts as docs

install and hardware:
 * [Ubuntu 22.04 LTS Installation on the Framework Laptop](https://guides.frame.work/Guide/Ubuntu+22.04+LTS+Installation+on+the+Framework+Laptop/109?lang=en)
 * docs: [hid-sensor](https://www.kernel.org/doc/html/latest/hid/hid-sensor.html) (`hid_sensor_hub` module)

power savings:
 * [Linux battery life tuning](https://community.frame.work/t/linux-battery-life-tuning/6665)
 * reddit: [tlp vs. power-profiles-daemon](https://www.reddit.com/r/Fedora/comments/qpaa4g/tlp_vs_powerprofilesdaemon/)

 stuttering/minifreezes:
 * [Periodic stuttering on fresh gnome.40 wayland install on Arch Linux](https://community.frame.work/t/periodic-stuttering-on-fresh-gnome-40-wayland-install-on-arch-linux/3912/5)
 * blog: [Fix for Intel i915 Freeze on Recent Linux Kernels](https://hobo.house/2018/05/18/fix-for-intel-i915-gpu-freeze-on-recent-linux-kernels/)
 * gist: [Brainiarc7](https://gist.github.com/Brainiarc7/aa43570f512906e882ad6cdd835efe57)
 * archwiki: [Intel graphics](https://wiki.archlinux.org/title/intel_graphics)


firefox:

 * archwiki: [Firefox](https://wiki.archlinux.org/title/Firefox#Hardware_video_acceleration)

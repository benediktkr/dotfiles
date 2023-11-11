#!/usr/bin/env python3

import shlex
import subprocess
import os


def run(args, env={}, error_ok=False):
    if isinstance(args, str):
        args = shlex.split(args)
    try:
        cmd = shlex.join(e.args)
    except AttributeError:
        cmd = " ".join([shlex.quote(a) for a in args])

    print(f"[>] {cmd}")

    try:
        p = subprocess.run(
            args,
            env={**os.environ, **env},
            capture_output=False,
            check=True
        )
    except subprocess.CalledProcessError as e:
        print(f"[!] returncode: {e.returncode}")
        if not error_ok:
            raise SystemError("aborted.")

def main():
    run("yadm submodule update --init --recursive")
    run("yadm submodule update --remote")

    home = os.path.expanduser("~")
    if not os.path.isdir(home + ".local/share/yadm/private/.git"):
        run([
            "git",
            "-C", home + ".local/share/yadm",
            clone,
            "https://git.sudo.is/ben/private-dotfiles"
            "private"
        ])

if __name__ == "__main__":
    main()



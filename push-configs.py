import argparse
import os

parser = argparse.ArgumentParser(description="push-configs.py")
parser.add_argument(
    "hosts",
    type=str,
    nargs='+',
    help="hosts to sync to")
parser.add_argument(
    "--emacs",
    action="store_true",
    help="push emacs config")
parser.add_argument(
    "--zsh",
    action="store_true",
    help="push zsh config")
parser.add_argument(
    "--dry-run",
    action="store_true",
    help="print scp commands, do not run them")
args = parser.parse_args()

emacs_config = {
    "emacs/.emacs": "~/.emacs",
    "emacs/.emacs.d/": "~/.emacs.d/",
}

zsh_config = {
    "zsh/.zshrc": "~/.zshrc",
}

def scp_cmds(host):
    configs = {}
    if args.emacs:
        configs.update(emacs_config)
    if args.zsh:
        configs.update(zsh_config)

    for local, remote in configs.items():
        yield "scp {local} {host}:{remote}".format(**locals())

if __name__ == "__main__":
    for hostname in args.hosts:
        for a in scp_cmds(hostname):
            if args.dry_run:
                print a
            else:
                print a
                os.system(a)

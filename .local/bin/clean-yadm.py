#!/usr/bin/env python3

import argparse
import subprocess
import time
import os

def prompt(question):
    print(f"[?] {question}?", end=" ")
    answer = input()
    if answer.lower() not in ["y", "yes"]:
        raise SystemExit("Aborting.")
    else:
        print("[ ] OK (sleeping 5s first)")
        time.sleep(5)

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--remove", action="store_true")
    parser.add_argument("-a", "--all", action="store_true")
    parser.add_argument("--no-dry-run", action="store_true")
    args = parser.parse_args()
    return args

def yadm_files(list_all=False):
    cmd_list = "yadm list".split(' ')
    if list_all:
        cmd_list.append("-a")

    print(f"[>] cmd: '{' '.join(cmd_list)}'")

    ps_list = subprocess.run(cmd_list, check=True, capture_output=True)
    return ps_list.stdout.decode().splitlines()


def get_yadm_paths():
    if 'XDG_DATA_HOME' in os.environ:
        yadm_data = os.path.join(os.environ['XDG_DATA_HOME'], 'yadm')
    else:
        yadm_data = os.path.join(os.environ['HOME'], '.local', 'share', 'yadm')

    if 'XDG_CONFIG_HOME' in os.environ:
        yadm_dir = os.path.join(os.environ['XDG_CONFIG_HOME'], 'yadm')
    else:
        yadm_dir = os.path.join(os.environ['HOME'], '.config', 'yadm')

    return {'yadm_dir': yadm_dir, 'yadm_data': yadm_data}

def main():
    args = parse_args()

    file_list = yadm_files(args.all)
    print("[!] Files managed by 'yadm':")
    for item in file_list:
        print(f"  {item}")

    yadm_paths = get_yadm_paths()
    yadm_data = yadm_paths['yadm_data']
    yadm_dir = yadm_paths['yadm_dir']
    print(f"[ ] YADM_DATA: {yadm_data}")
    print(f"[ ] YADM_DIR: {yadm_dir}")

    if args.no_dry_run:
        print("[!] This is NOT A DRY RUN, FILES WILL BE REMOVED")
    else:
        print("[ ] This is a dry run, files will not be removed")

    prompt("Remove files")

    for item in file_list:
        print(f"rm: '{item}'")
        if args.no_dry_run:
            os.remove(item)

    yadm_repo_path = os.path.join(yadm_data, "repo.git")
    prompt(f"Delete repo at '{yadm_repo_path}'")
    print(f"rm: '{yadm_repo_path}'")
    if args.no_dry_run:
        os.rmtree(yadm_repo_path)

    if not args.no_dry_run:
        print("[ ] This was a dry run, nothing was removed")


if __name__ == "__main__":
    main()







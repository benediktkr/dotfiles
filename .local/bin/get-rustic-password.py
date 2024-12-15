#!/usr/bin/env python3

from argparse import ArgumentParser
import os


def parse_args():
    parser = ArgumentParser()
    parser.add_argument("-P", "--use-profile", help="Use rustic config profile name", default="rustic")
    return parser.parse_args()


def read_password_file(profile_name):
    passwd_file = f"{profile_name}.txt"
    passwd_path = os.path.join(os.getenv("HOME"), ".config/rustic/", passwd_file)
    try:
        with open(passwd_path, 'r') as f:
            return f.read().strip()
    except FileNotFoundError as e:
        raise SystemExit(e)


def main():
    args = parse_args()
    if "RUSTIC_PASSWORD" in os.environ:
        passw = os.getenv("RUSTIC_PASSWORD")
    else:
        passwd = read_password_file(args.use_profile)

    print(passwd, end="")


if __name__ == "__main__":
    main()

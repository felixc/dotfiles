#!/usr/bin/python3

import hashlib
import sys
import urllib.request


def check_password(password):
    digest = hashlib.sha1(password.encode()).hexdigest().upper()
    prefix, suffix = digest[:5], digest[5:]

    request = urllib.request.Request(
        "https://api.pwnedpasswords.com/range/{}".format(prefix),
        headers={"User-Agent": "check-passwords"},
    )

    with urllib.request.urlopen(request) as response:
        for line in response.readlines():
            line = line.decode().strip()
            if line.split(":")[0] == suffix:
                print("Compromised: {}".format(password))
                break


def check_passwords(input_stream):
    for password in input_stream:
        check_password(password.strip())


if __name__ == "__main__":
    check_passwords(sys.stdin)

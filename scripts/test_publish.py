#!/usr/bin/env python3

# -------------------------------------------------------------------
# This file is part of the CernVM File System.
# -------------------------------------------------------------------

from base64 import b64encode
from optparse import OptionParser
from urllib.request import urlopen
import requests


def do_trigger(url, repo, manifest, verbose):
    data = {'version': 1,
            'timestamp': '19 Sep 2018 13:14:15',
            'type': 'activity',
            'repository': repo,
            'manifest': b64encode(manifest).decode('utf-8')}
    headers = {'Content-type': 'application/json'}
    reply = requests.post('http://' + url, json=data, headers=headers)

    if verbose:
        print('Reply: status: {}, json: {}'.format(
            reply.status_code, reply.json()))
    else:
        print(reply.json())


def main():
    usage = "{}\n\n{}".format("Usage: %prog [options] URL REPO_NAME MANIFEST",
                              "Ex: %prog localhost:8081/api/v1/trigger my_repo http://hostname.cern.ch/cvmfs/my_repo/.cvmfspublished")

    parser = OptionParser(usage)
    parser.add_option("-v", "--verbose", dest="verbose", action="store_true",
                      help="verbose output")

    (options, args) = parser.parse_args()
    if len(args) != 3:
        parser.error("incorrect number of arguments")

    url = args[0]
    repo_name = args[1]

    with urlopen(args[2]) as f:
        manifest = f.read()

    if options.verbose:
        print("Configuration:")
        print("  notification service URL: {}".format(url))
        print("  repository name: {}".format(repo_name))
        print("  manifest: {}".format(manifest))
        print()

    do_trigger(url, repo_name, manifest, options.verbose)


if __name__ == '__main__':
    main()

#!/usr/bin/env python3

###-------------------------------------------------------------------
### This file is part of the CernVM File System.
###-------------------------------------------------------------------

from optparse import OptionParser
import requests

def do_trigger(url, repo, revision, root_hash, verbose):
    if verbose:
        print('Trigger: ', repo, revision, root_hash)

    data = {'repo' : repo,
            'revision' : revision,
            'root_hash' : root_hash,
            'api_version' : 1}
    headers = {'Content-type' : 'application/json'}
    reply = requests.post('http://' + url, json = data, headers = headers)

    if verbose:
        print('Reply: status: {}, json: {}'.format(reply.status_code, reply.json()))
    else:
        print(reply.json())

def main():
    usage = "{}\n\n{}".format("Usage: %prog [options] URL REPO_NAME REVISION ROOT_HASH",
                            "Ex: %prog localhost:8081/api/v1/trigger my_repo 17 abcdef")

    parser = OptionParser(usage)
    parser.add_option("-v", "--verbose", dest="verbose", action="store_true",
                      help="verbose output")

    (options, args) = parser.parse_args()
    if len(args) != 4 :
        parser.error("incorrect number of arguments")

    url = args[0]
    repo_name = args[1]
    revision = args[2]
    root_hash = args[3]

    if options.verbose:
        print("Configuration:")
        print("  notification service URL: {}".format(url))
        print("  repository name: {}".format(repo_name))
        print("  revision: {}".format(revision))
        print("  root hash: {}".format(root_hash))
        print()

    do_trigger(url, repo_name, revision, root_hash, options.verbose)

if __name__ == '__main__':
    main()

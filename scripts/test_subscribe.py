#!/usr/bin/env python3

###-------------------------------------------------------------------
### This file is part of the CernVM File System.
###-------------------------------------------------------------------

import asyncio
import json
import websockets
from optparse import OptionParser

async def do_request(url, repo_name, continous, verbose):
    msg = str.encode(json.dumps({'version' : 1,
                                 'repository' : repo_name}))

    async with websockets.connect(url) as websocket:
        if verbose:
            print("Subscription request sent")

        await websocket.send(msg)

        while True:
            reply = await websocket.recv()
            if verbose:
                print("Notification received: {}".format(json.loads(reply.decode())))
            else:
                print(reply.decode())
            if not continous:
                break


def listen(url, repo_name, continuous, verbose):
    asyncio.get_event_loop().run_until_complete(
        do_request('ws://' + url, repo_name, continuous, verbose)
    )


def main():
    usage = "{}\n\n{}".format("Usage: %prog [options] URL REPO_NAME",
                              "Ex: %prog localhost:8081/api/v1/subcribe my_repo")
    parser = OptionParser(usage)
    parser.add_option("-v", "--verbose", dest="verbose", action="store_true",
                      help="verbose output")
    parser.add_option("-c", "--continuous", dest="continuous", action="store_true",
                      help="continuous mode: do not exit after the first notification "
                      + "is received or timeout is reached")

    (options, args) = parser.parse_args()
    if len(args) != 2 :
        parser.error("incorrect number of arguments")

    url = args[0]
    repo_name = args[1]

    if options.verbose:
        print("Configuration:")
        print("  notification service URL: {}".format(url))
        print("  repository name: {}".format(repo_name))
        print()

    listen(url, repo_name, options.continuous, options.verbose)

if __name__ == '__main__':
    main()

#!/usr/bin/env python3

###-------------------------------------------------------------------
### This file is part of the CernVM File System.
###-------------------------------------------------------------------

import asyncio
import json
import websockets

async def hello(uri, frame):
    async with websockets.connect(uri) as websocket:
        print("Frame sent: {}".format(frame))
        await websocket.send(frame)
        reply = await websocket.recv()
        print("Frame received: {}".format(json.loads(reply.decode())))
        # try:
        #     pong = await websocket.ping()
        #     await asyncio.wait_for(pong, timeout=10)
        # except asyncio.TimeoutError:
        #     print("Timeout reached while waiting for pong")
        # print("Pong received")

subscription_msg = json.dumps({'repo' : 'test_repo', 'min_revision' : 0 })
asyncio.get_event_loop().run_until_complete(
    hello('ws://localhost:8081/api/v1/notify', str.encode(subscription_msg)))

###-------------------------------------------------------------------
### This file is part of the CernVM File System.
###-------------------------------------------------------------------

#!/usr/bin/env python3

import asyncio
import websockets

async def hello(uri, frame):
    async with websockets.connect(uri) as websocket:
        print("Frame sent: {}".format(frame))
        await websocket.send(frame)
        reply = await websocket.recv()
        print("Frame received: {}".format(reply))

asyncio.get_event_loop().run_until_complete(
    hello('ws://localhost:8081/api/v1', "Hello websocket server!"))

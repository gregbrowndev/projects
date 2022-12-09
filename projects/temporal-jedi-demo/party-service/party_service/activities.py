from asyncio import sleep
from random import randrange

from temporalio import activity


@activity.defn
async def execute_order_67() -> int:
    await sleep(5)
    troopers_danced = randrange(3, 10)
    return troopers_danced

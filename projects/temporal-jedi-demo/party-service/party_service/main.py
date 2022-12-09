import asyncio
import logging

from temporalio.client import Client
from temporalio.worker import Worker

from party_service.activities import say_hello

TEMPORAL_SERVER_URL = "localhost:7322"
TASK_QUEUE = "jedi-demo"

logger = logging.getLogger(__name__)


async def main() -> None:
    client = await Client.connect(TEMPORAL_SERVER_URL)
    worker = Worker(client, task_queue=TASK_QUEUE, workflows=[], activities=[say_hello])
    logger.info("Worker Created")
    await worker.run()


if __name__ == "__main__":
    logger.info("Running Temporal Worker")
    asyncio.run(main())

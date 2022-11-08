import { NativeConnection, Worker } from '@temporalio/worker';
import * as activities from './activities';

run().catch((err) => console.log(err));

export const TASK_QUEUE = 'tutorial';
export const TEMPORAL_URL = 'localhost:7233';

async function run() {
  const worker = await Worker.create({
    connection: await NativeConnection.connect({ address: TEMPORAL_URL }),
    workflowsPath: require.resolve('./workflows'), // passed to Webpack for bundling
    activities, // directly imported in Node.js
    taskQueue: TASK_QUEUE,
  });
  await worker.run();
}

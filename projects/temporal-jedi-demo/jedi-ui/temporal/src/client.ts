import { Connection, WorkflowClient } from '@temporalio/client';
import { TEMPORAL_URL } from './worker';

export default async function createClient(): Promise<WorkflowClient> {
  return new WorkflowClient({
    connection: await Connection.connect({
      address: TEMPORAL_URL,
    }),
  });
}

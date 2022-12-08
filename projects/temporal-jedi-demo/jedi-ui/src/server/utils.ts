import { WorkflowClient } from '@temporalio/client';

const TEMPORAL_URL = 'localhost:7233';

export async function createWorkflowClient(): Promise<WorkflowClient> {
  return new WorkflowClient(/*{
    connection: await Connection.connect({
      address: TEMPORAL_URL,
    }),
  }*/);
}

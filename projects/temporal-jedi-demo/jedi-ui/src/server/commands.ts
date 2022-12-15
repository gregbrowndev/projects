import { createWorkflowClient } from './utils';
import * as proto from '@temporalio/proto';
import { Order, SendOrderData, StartWorkflowData } from './types';
import { jediBusiness, orderSignal } from '../temporal/src/workflows';
import { v4 as uuidv4 } from 'uuid';
import { TASK_QUEUE } from '../temporal/src/worker';

export const WorkflowExecutionStatus =
  proto.temporal.api.enums.v1.WorkflowExecutionStatus;

export async function startWorkflow(): Promise<StartWorkflowData> {
  const client = await createWorkflowClient();
  const workflowId = uuidv4();
  const handle = await client.start(jediBusiness, {
    workflowId: workflowId,
    taskQueue: TASK_QUEUE,
    args: [],
  });
  return { workflowId: handle.workflowId };
}

export async function deleteWorkflow(workflowId: string): Promise<void> {
  const client = await createWorkflowClient();
  const workflow = client.getHandle(workflowId);
  const workflowDescription = await workflow.describe();
  if (
    workflowDescription.status.code ==
    WorkflowExecutionStatus.WORKFLOW_EXECUTION_STATUS_RUNNING
  ) {
    console.log(`Terminating workflow: ${workflowId}`);
    await workflow.terminate();
  }
}

export async function sendOrder(
  workflowId: string,
  order: Order,
): Promise<SendOrderData> {
  const client = await createWorkflowClient();
  const workflow = client.getHandle(workflowId);
  await workflow.signal(orderSignal, order);
  return { workflowId: workflowId };
}

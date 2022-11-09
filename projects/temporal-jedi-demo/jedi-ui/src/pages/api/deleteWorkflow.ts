import type { NextApiRequest, NextApiResponse } from 'next';
import {
  deleteWorkflowId,
  createClient,
  ErrorData,
  getWorkflowId,
} from './utils';
import * as proto from '@temporalio/proto';

const WorkflowExecutionStatus =
  proto.temporal.api.enums.v1.WorkflowExecutionStatus;

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<null | ErrorData>,
) {
  const client = await createClient();

  let workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    res.status(400).json({ type: 'error', message: 'No workflow found' });
    return;
  }

  // Delete workflowID cookie so UI can return to start page
  deleteWorkflowId({ req, res });

  const workflow = client.getHandle(workflowId);
  const workflowDescription = await workflow.describe();
  if (
    workflowDescription.status.code ==
    WorkflowExecutionStatus.WORKFLOW_EXECUTION_STATUS_RUNNING
  ) {
    console.log(`Terminating workflow: ${workflowId}`);
    await workflow.terminate();
  }

  res.status(204).send(null);
}

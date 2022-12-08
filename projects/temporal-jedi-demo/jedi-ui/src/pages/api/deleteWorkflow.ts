import type { NextApiRequest, NextApiResponse } from 'next';
import { ErrorData } from '../../server/types';
import { deleteWorkflowId, getWorkflowId } from '../../server/cookies';
import { deleteWorkflow } from '../../server/commands';

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<null | ErrorData>,
) {
  let workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    res
      .status(400)
      .json({ type: 'error', message: 'No workflow found', detail: null });
    return;
  }

  await deleteWorkflow(workflowId);
  deleteWorkflowId({ req, res }); // Delete workflowID cookie
  res.status(204).send(null);
}

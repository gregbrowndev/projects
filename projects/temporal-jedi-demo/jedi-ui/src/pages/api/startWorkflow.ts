import type { NextApiRequest, NextApiResponse } from 'next';
import { ErrorData, StartWorkflowData } from '../../server/types';
import { getWorkflowId, setWorkflowId } from '../../server/cookies';
import { startWorkflow } from '../../server/commands';

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<StartWorkflowData | ErrorData>,
) {
  let workflowId = getWorkflowId({ req, res });
  if (workflowId) {
    res.status(400).json({
      type: 'error',
      message: 'Workflow already started',
      detail: null,
    });
    return;
  }

  const workflowData = await startWorkflow();
  setWorkflowId(workflowData.workflowId, { req, res }); // set cookie
  res.status(200).json(workflowData);
}

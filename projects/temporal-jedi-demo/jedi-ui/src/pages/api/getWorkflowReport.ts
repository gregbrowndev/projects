import type { NextApiRequest, NextApiResponse } from 'next';
import { getWorkflowId } from '../../server/utils';
import { ErrorData, WorkflowReportData } from '../../server/types';
import { getWorkflowReport } from '../../server/queries';

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<WorkflowReportData | ErrorData>,
) {
  const workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    res
      .status(400)
      .json({ type: 'error', message: 'Workflow not started', detail: null });
    return;
  }

  const report = await getWorkflowReport(workflowId);
  res.status(200).json(report);
}

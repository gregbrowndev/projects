import type { NextApiRequest, NextApiResponse } from 'next';
import { ErrorData, OrderReportData } from '../../server/types';
import { getOrderReport } from '../../server/queries';
import { getWorkflowId } from '../../server/cookies';

export default async function handler(
  req: NextApiRequest,
  res: NextApiResponse<OrderReportData | {} | ErrorData>,
) {
  const workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    res.status(400).json({ type: 'error', message: 'Workflow not started' });
    return;
  }

  const report = await getOrderReport(workflowId);
  res.status(200).json(report || {});
}

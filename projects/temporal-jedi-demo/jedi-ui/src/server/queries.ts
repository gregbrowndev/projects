import { OrderReportData, WorkflowReportData } from './types';
import { createWorkflowClient } from './utils';
import {
  orderReportQuery,
  workflowReportQuery,
} from '../temporal/src/workflows';

export async function getOrderReport(
  workflowId: string,
): Promise<OrderReportData | undefined> {
  const client = await createWorkflowClient();

  const workflow = client.getHandle(workflowId);
  const report = await workflow.query(orderReportQuery);

  if (!report) {
    return undefined;
  }

  return {
    workflowId: workflowId,
    type: report.type,
    status: report.status,
    troopersDanced: report.troopersDanced || null,
    jediEliminated: report.jediEliminated || null,
  };
}

export async function getWorkflowReport(
  workflowId: string,
): Promise<WorkflowReportData> {
  const client = await createWorkflowClient();

  const workflow = client.getHandle(workflowId);
  const report = await workflow.query(workflowReportQuery);

  return {
    workflowId: workflowId,
    workflowComplete: report.workflowComplete,
    currentOrderStatus: report.currentOrderStatus,
    troopersDanced: report.troopersDanced,
    jediEliminated: report.jediEliminated,
    jediRemaining: report.jediRemaining,
  };
}

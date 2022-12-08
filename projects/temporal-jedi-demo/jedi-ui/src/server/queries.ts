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
    totalTroopersDanced: report.totalTroopersDanced || null,
    jediEliminated: report.jediEliminated || null,
    totalJediEliminated: report.totalJediEliminated || null,
    totalJediRemaining: report.totalJediRemaining || null,
  };
}

export async function getWorkflowReport(
  workflowId: string,
): Promise<WorkflowReportData> {
  console.log(
    `[queries] getWorkflowReport called with workflowId='${workflowId}'`,
  );
  const client = await createWorkflowClient();
  console.log(`[queries] getWorkflowReport created WorkflowClient`);

  const workflow = client.getHandle(workflowId);
  const report = await workflow.query(workflowReportQuery);
  console.log(`[queries] getWorkflowReport received query result`, report);

  return {
    workflowId: workflowId,
    workflowStatus: report.workflowStatus,
    currentOrderStatus: report.currentOrderStatus || null,
    troopersDanced: report.troopersDanced,
    jediEliminated: report.jediEliminated,
    jediRemaining: report.jediRemaining,
  };
}

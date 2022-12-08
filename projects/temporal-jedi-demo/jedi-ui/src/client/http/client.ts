import { getCookie } from 'cookies-next';
import {
  ErrorData,
  Order,
  OrderReportData,
  SendOrderData,
  StartWorkflowData,
  WorkflowReportData,
} from '../../server/types';

// Local cookie storage

export function getWorkflowIdCookie(): string | undefined {
  const cookieVal = getCookie('workflowId');
  if (typeof cookieVal == 'string') {
    console.log(`Found workflow ID: ${cookieVal}`);
    return cookieVal;
  }
}

// Workflow management API

export async function startWorkflow(): Promise<StartWorkflowData | ErrorData> {
  return fetch('/api/startWorkflow', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
  }).then((res) => {
    return res.json();
  });
}

export async function deleteWorkflow(): Promise<void> {
  await fetch('/api/deleteWorkflow', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
  });
}

export async function getWorkflowReport(): Promise<
  WorkflowReportData | ErrorData
> {
  return fetch('/api/getWorkflowReport', {
    method: 'GET',
    headers: { 'Content-Type': 'application/json' },
  }).then((res) => {
    return res.json();
  });
}

// Order API

export async function sendOrder(
  order: Order,
): Promise<SendOrderData | ErrorData> {
  return fetch('/api/sendOrder', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(order),
  }).then((res) => {
    return res.json();
  });
}

export async function getOrderReport(): Promise<OrderReportData | ErrorData> {
  return fetch('/api/getOrderReport', {
    method: 'GET',
    headers: { 'Content-Type': 'application/json' },
  }).then((res) => {
    return res.json();
  });
}

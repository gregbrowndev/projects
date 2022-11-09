import { getCookie } from 'cookies-next';
import { ErrorData } from '../pages/api/utils';

// Seeing some perplexing issues due to Temporal. Avoid importing any modules that depend on Temporal
// This is probably because these modules can't be loaded client-side
import {
  GetOrderStatusData,
  GetTeaDrunkData,
  Order,
  SendOrderData,
  StartWorkflowData,
} from './hack';
// import { Order, OrderStatus } from '../temporal/src/workflows';
// import { GetOrderStatusData } from '../pages/api/getOrderStatus';
// import { StartWorkflowData } from '../pages/api/startWorkflow';
// import { ErrorData } from '../pages/api/utils';
// import { GetTeaDrunkData } from '../pages/api/getTeaDrunk';
// import { SendOrderData } from '../pages/api/sendOrder';

export function getWorkflowIdCookie(): string | undefined {
  const cookieVal = getCookie('workflowId');
  if (typeof cookieVal == 'string') {
    console.log(`Found workflow ID: ${cookieVal}`);
    return cookieVal;
  }
}

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

export async function getOrderStatus(): Promise<
  GetOrderStatusData | ErrorData
> {
  // return fetch('/api/getOrderStatus', {
  //   method: 'GET',
  //   headers: { 'Content-Type': 'application/json' },
  // }).then((res) => {
  //   return res.json();
  // });
  return { workflowId: 'asdasdas', orderStatus: 'EXECUTING' };
}

export async function getTeaDrunk(): Promise<GetTeaDrunkData | ErrorData> {
  return fetch('/api/getTeaDrunk', {
    method: 'GET',
    headers: { 'Content-Type': 'application/json' },
  }).then((res) => {
    return res.json();
  });
}

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

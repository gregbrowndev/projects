import * as wf from '@temporalio/workflow';
import type * as activities from './activities'; // purely for type safety

import { sleep } from '@temporalio/workflow';

const { executeOrder } = wf.proxyActivities<typeof activities>({
  startToCloseTimeout: '1 minute',
});

export type OrderType = 'Order66' | 'Order67';

// Define signals and queries
export interface Order {
  type: OrderType;
  fromUser: string;
}
export type OrderStatus = 'WAITING' | 'EXECUTING';
export interface OrderReport {
  type: OrderType;
  status: OrderStatus;
  troopersDanced?: number;
  jediEliminated?: number;
}
export interface WorkflowReport {
  workflowComplete: boolean;
  currentOrderStatus: OrderStatus;
  troopersDanced: number;
  jediEliminated: number;
  jediRemaining: number;
}

export const orderSignal = wf.defineSignal<[Order]>('order');
export const orderReportQuery = wf.defineQuery<OrderReport | undefined>(
  'orderReportQuery',
);
export const workflowReportQuery = wf.defineQuery<WorkflowReport>(
  'workflowReportQuery',
);

export async function jediBusiness(): Promise<void> {
  const totalJedi = 10;
  let troopersDanced = 0;
  let jediEliminated = 0;

  let currentOrder: Order | undefined;
  let currentOrderReport: OrderReport | undefined;

  wf.setHandler(orderSignal, (order) => {
    if (currentOrder) {
      throw new Error('Cannot do that right now');
    }
    currentOrder = order;
    currentOrderReport = {
      type: order.type,
      status: 'EXECUTING',
    };
  });
  wf.setHandler(orderReportQuery, () => currentOrderReport);
  wf.setHandler(workflowReportQuery, () => {
    console.log('Handling workflowReportQuery');
    return {
      workflowComplete: totalJedi - jediEliminated <= 0,
      troopersDanced,
      jediEliminated,
      jediRemaining: Math.max(totalJedi - jediEliminated, 0),
      currentOrderStatus: currentOrderReport?.status || 'WAITING',
    };
  });

  while (jediEliminated > 0) {
    await wf.condition(() => currentOrder !== undefined);
    if (currentOrder == undefined || currentOrderReport == undefined) {
      throw new Error('Something went wrong');
    }

    currentOrderReport.status = 'EXECUTING';
    console.log(`Handling Order: ${currentOrder.type}!`);

    if (currentOrder.type == 'Order66') {
      await executeOrder(currentOrder.type);
      await sleep(5000);
      currentOrderReport.jediEliminated = 3;
      jediEliminated += 3;
    } else {
      await executeOrder(currentOrder.type);
      await sleep(5000);
      currentOrderReport.troopersDanced = 5;
      troopersDanced += 5;
    }

    currentOrderReport.status = 'WAITING';
  }
}

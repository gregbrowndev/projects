import * as wf from '@temporalio/workflow';
import type * as activities from './activities';
import {sleep} from "@temporalio/workflow"; // purely for type safety

const { executeOrder } = wf.proxyActivities<typeof activities>({
  startToCloseTimeout: '1 minute',
});

// Define signals and queries
export interface Order {
  type: 'Order66' | 'Order67';
  fromUser: string;
}
export const orderSignal = wf.defineSignal<[Order]>('order');

export type OrderStatus = 'WAITING' | 'EXECUTING' | 'DONE';
export const orderStatusQuery = wf.defineQuery<OrderStatus>('orderStatus');

export const teaDrunkQuery = wf.defineQuery<number>("teaDrunkQuery")

export async function jediBusiness(): Promise<void> {
  let awaitingOrder66 = true;
  let orderStatus: OrderStatus = 'WAITING';
  let orders: Order[] = [];
  let teaDrunk = 0

  wf.setHandler(orderSignal, (order) => {
    orders = [...orders, order];
  });
  wf.setHandler(orderStatusQuery, () => orderStatus);
  wf.setHandler(teaDrunkQuery, () => teaDrunk);

  while (awaitingOrder66) {
    await wf.condition(() => orders.length > 0);
    // TODO - use .then() syntax?
    const order = orders.shift();
    if (!order) continue;

    orderStatus = 'EXECUTING';
    console.log(`Handling Order: ${order.type}!`);

    if (order.type == 'Order66') {
      await executeOrder(order.type);
      awaitingOrder66 = false
    } else {
      await executeOrder(order.type);
      ++teaDrunk
    }

    await sleep(5000);
    orderStatus = 'WAITING';
  }

  orderStatus = 'DONE';
}

import * as wf from '@temporalio/workflow';
import { sleep } from '@temporalio/workflow';
import type * as activities from './activities'; // purely for type safety
import { Order, OrderReport, WorkflowReport, WorkflowStatus } from './types';

const { executeOrder } = wf.proxyActivities<typeof activities>({
  startToCloseTimeout: '1 minute',
});
export const orderSignal = wf.defineSignal<[Order]>('order');
export const orderReportQuery = wf.defineQuery<OrderReport | undefined>(
  'orderReportQuery',
);
export const workflowReportQuery = wf.defineQuery<WorkflowReport>(
  'workflowReportQuery',
);

export async function jediBusiness(): Promise<void> {
  let state: State = {
    totalJedi: 10,
    troopersDanced: 0,
    jediEliminated: 0,
  };

  wf.setHandler(orderSignal, (order) => {
    state = setOrder(state, order);
  });
  wf.setHandler(orderReportQuery, () => state.currentOrderReport);
  wf.setHandler(workflowReportQuery, () => getWorkflowReport(state));

  while (getJediRemaining(state) > 0) {
    await wf.condition(() => hasOrder(state));
    if (!state.currentOrder || !state.currentOrderReport) {
      throw new Error('Something went wrong');
    }
    const order = state.currentOrder;

    state = setOrderExecuting(state);
    console.log(`Handling Order: ${order.type}!`);

    if (state.currentOrder?.type == 'Order66') {
      await executeOrder(order.type);
      await sleep(5000);
      state = addJediEliminated(state, 3);
    } else {
      await executeOrder(order.type);
      await sleep(5000);
      state = addTroopersDanced(state, 3);
    }

    state = setOrderDone(state);
  }
}

interface State {
  totalJedi: number;
  troopersDanced: number;
  jediEliminated: number;
  currentOrder?: Order;
  currentOrderReport?: OrderReport;
}

function addTroopersDanced(state: State, troopers: number): State {
  if (!state.currentOrderReport) {
    throw new Error('Cannot do that right now');
  }
  return {
    ...state,
    troopersDanced: state.troopersDanced + troopers,
    currentOrderReport: {
      ...state.currentOrderReport,
      troopersDanced: troopers,
    },
  };
}
function addJediEliminated(state: State, jedi: number): State {
  if (!state.currentOrderReport) {
    throw new Error('Cannot do that right now');
  }
  const totalEliminations = state.jediEliminated + jedi;
  return {
    ...state,
    jediEliminated: Math.min(state.totalJedi, totalEliminations),
    currentOrderReport: {
      ...state.currentOrderReport,
      jediEliminated: jedi,
    },
  };
}
function getJediRemaining(state: State): number {
  return state.totalJedi - state.jediEliminated;
}

function getWorkflowStatus(state: State): WorkflowStatus {
  if (getJediRemaining(state) == 0) return 'DONE';
  else if (state.currentOrderReport?.status == 'EXECUTING') return 'EXECUTING';
  else return 'WAITING';
}

function getWorkflowReport(state: State): WorkflowReport {
  console.log('Handling workflowReportQuery');

  return {
    workflowStatus: getWorkflowStatus(state),
    troopersDanced: state.troopersDanced,
    jediEliminated: state.jediEliminated,
    jediRemaining: getJediRemaining(state),
    currentOrderStatus: state.currentOrderReport?.status,
  };
}

function hasOrder(state: State): boolean {
  return state.currentOrder != undefined;
}

function setOrder(state: State, order: Order): State {
  if (state.currentOrder) {
    throw new Error('Cannot do that right now');
  }
  return {
    ...state,
    currentOrder: order,
    currentOrderReport: {
      type: order.type,
      status: 'EXECUTING',
    },
  };
}

function setOrderExecuting(state: State): State {
  if (!state.currentOrder || !state.currentOrderReport) {
    throw new Error('Cannot do that right now');
  }
  return {
    ...state,
    currentOrderReport: {
      ...state.currentOrderReport,
      status: 'EXECUTING',
    },
  };
}

function setOrderDone(state: State): State {
  if (!state.currentOrder || !state.currentOrderReport) {
    throw new Error('Cannot do that right now');
  }
  return {
    ...state,
    currentOrder: undefined,
    currentOrderReport: {
      ...state.currentOrderReport,
      status: 'DONE',
    },
  };
}

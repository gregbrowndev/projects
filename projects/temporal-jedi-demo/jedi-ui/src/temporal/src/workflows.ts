import * as wf from '@temporalio/workflow';
import type * as activities from './activities'; // purely for type safety
import { Order, OrderReport, WorkflowReport, WorkflowStatus } from './types';

const { executeOrder66, executeOrder67 } = wf.proxyActivities<
  typeof activities
>({
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
    order66Count: 0,
    order67Count: 0,
  };

  wf.setHandler(orderSignal, (order) => {
    state = setOrder(state, order);
  });
  wf.setHandler(orderReportQuery, () => getOrderReport(state));
  wf.setHandler(workflowReportQuery, () => getWorkflowReport(state));

  while (getJediRemaining(state) > 0) {
    await wf.condition(() => hasOrder(state));
    if (!state.currentOrder || !state.currentOrderReport) {
      throw new Error('Something went wrong');
    }
    const order = state.currentOrder;
    console.log(`Handling Order: ${order.type}!`);
    state = setOrderExecuting(state);

    if (order.type == 'Order66') {
      const jediEliminated = await executeOrder66(order.type);
      state = addJediEliminated(state, jediEliminated);
    } else {
      const troopersDanced = await executeOrder67(order.type);
      state = addTroopersDanced(state, troopersDanced);
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
  order66Count: number;
  order67Count: number;
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
function addJediEliminated(state: State, jediEliminated: number): State {
  if (!state.currentOrderReport) {
    throw new Error('Cannot do that right now');
  }
  const jediRemaining = getJediRemaining(state);
  jediEliminated = Math.min(jediEliminated, jediRemaining);
  const totalEliminations = state.jediEliminated + jediEliminated;
  return {
    ...state,
    jediEliminated: totalEliminations,
    currentOrderReport: {
      ...state.currentOrderReport,
      jediEliminated: jediEliminated,
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
  return {
    workflowStatus: getWorkflowStatus(state),
    troopersDanced: state.troopersDanced,
    jediEliminated: state.jediEliminated,
    jediRemaining: getJediRemaining(state),
    currentOrderStatus: state.currentOrderReport?.status,
  };
}

function getOrderReport(state: State): OrderReport {
  if (!state.currentOrderReport) {
    throw new Error('Cannot do that right now');
  }
  const workflowReport = getWorkflowReport(state);
  return {
    ...state.currentOrderReport,
    totalTroopersDanced: workflowReport.troopersDanced,
    totalJediEliminated: workflowReport.jediEliminated,
    totalJediRemaining: workflowReport.jediRemaining,
  };
}

function hasOrder(state: State): boolean {
  return state.currentOrder != undefined;
}

function setOrder(state: State, order: Order): State {
  if (state.currentOrder) {
    throw new Error('Cannot do that right now');
  }
  const orderCountByType =
    order.type == 'Order66' ? ++state.order66Count : ++state.order67Count;

  return {
    ...state,
    currentOrder: order,
    order66Count:
      order.type == 'Order66' ? orderCountByType : state.order66Count,
    order67Count:
      order.type == 'Order67' ? orderCountByType : state.order67Count,
    currentOrderReport: {
      type: order.type,
      status: 'EXECUTING',
      orderCountByType,
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

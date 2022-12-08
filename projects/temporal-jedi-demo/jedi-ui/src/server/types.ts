// Note: you must not import any types from the temporal subproject because
// Webpack may try to bundle Temporal and send it to the client-side application

export type OrderType = 'Order66' | 'Order67';
export type OrderStatus = 'EXECUTING' | 'DONE';
export type WorkflowStatus = 'WAITING' | 'EXECUTING' | 'DONE';

export type StartWorkflowData = {
  workflowId: string;
};

export interface Order {
  type: 'Order66' | 'Order67';
  fromUser: string;
}

export type SendOrderData = {
  workflowId: string;
};

export type OrderReportData = {
  workflowId: string;
  type: OrderType;
  status: OrderStatus;
  troopersDanced: number | null;
  jediEliminated: number | null;
};

export type WorkflowReportData = {
  workflowId: string;
  workflowStatus: WorkflowStatus;
  currentOrderStatus: OrderStatus | null;
  troopersDanced: number;
  jediEliminated: number;
  jediRemaining: number;
};

export interface ErrorData {
  type: 'error';
  message: string;
  detail: string | null;
}

export function isErrorData(object: any): object is ErrorData {
  return 'type' in object && object.type == 'error';
}

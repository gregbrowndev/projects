import { OrderStatus, OrderType } from '../temporal/src/workflows';

export type OrderReportData = {
  workflowId: string;
  type: OrderType;
  status: OrderStatus;
  troopersDanced: number | null;
  jediEliminated: number | null;
};

export type WorkflowReportData = {
  workflowId: string;
  currentOrderStatus: OrderStatus;
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
  return 'type' in object && object.type == 'Error';
}

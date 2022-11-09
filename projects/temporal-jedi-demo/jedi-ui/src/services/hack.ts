// Redefine all the API data types clientside to avoid Temporal bundling issues
import { ErrorData } from '../pages/api/utils';

export interface Order {
  type: 'Order66' | 'Order67';
  fromUser: string;
}

export type OrderStatus = 'WAITING' | 'EXECUTING' | 'DONE';
export type GetOrderStatusData = {
  workflowId: string;
  orderStatus: OrderStatus;
};
export type GetTeaDrunkData = {
  workflowId: string;
  teaDrunk: number;
};
export type SendOrderData = {
  workflowId: string;
};
export type StartWorkflowData = {
  workflowId: string;
};

export function isErrorData(object: any): object is ErrorData {
  return 'type' in object && object.type == 'Error';
}

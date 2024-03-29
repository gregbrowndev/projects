export type OrderType = 'Order66' | 'Order67';

export interface Order {
  type: OrderType;
  fromUser: string;
}

export type OrderStatus = 'EXECUTING' | 'DONE';
export type WorkflowStatus = 'WAITING' | 'EXECUTING' | 'DONE';

export interface OrderReport {
  type: OrderType;
  status: OrderStatus;
  orderCountByType: number;
  troopersDanced?: number;
  totalTroopersDanced?: number;
  jediEliminated?: number;
  totalJediEliminated?: number;
  totalJediRemaining?: number;
}

export interface WorkflowReport {
  workflowStatus: WorkflowStatus;
  currentOrderStatus?: OrderStatus;
  troopersDanced: number;
  jediEliminated: number;
  jediRemaining: number;
}

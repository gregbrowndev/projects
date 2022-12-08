import { Order, WorkflowReportData } from '../../../server/types';
import React from 'react';
import Button from '../system/Button';
import { Card } from '../system/Card';
import { StatsList } from './StatsList';

export interface WorkflowReportCardProps {
  workflowReport: WorkflowReportData;
  onSendOrder: (order: Order) => {};
  submittingOrder: Order | undefined;
  currentUser: string;
}
export const WorkflowReportCard: React.FC<WorkflowReportCardProps> = ({
  workflowReport,
  onSendOrder,
  submittingOrder,
  currentUser,
}) => {
  return (
    <Card
      title="Awaiting an Order"
      body={
        <StatsList
          items={
            <>
              <li>Troopers Danced: {workflowReport.troopersDanced}</li>
              <li>Jedi Eliminated: {workflowReport.jediEliminated}</li>
              <li>Jedi Remaining: {workflowReport.jediRemaining}</li>
            </>
          }
        />
      }
      footer={
        <>
          {/* TODO - add transition, hide the button not clicked and centre other */}
          <Button
            type="button"
            variant="secondary"
            size="large"
            onClick={() =>
              onSendOrder({ type: 'Order66', fromUser: currentUser })
            }
            label={
              submittingOrder?.type == 'Order66' ? 'Submitting' : 'Order 66'
            }
            disabled={submittingOrder != null}
            loading={submittingOrder?.type == 'Order66'}
          />
          <Button
            type="button"
            variant="secondary"
            size="large"
            onClick={() =>
              onSendOrder({ type: 'Order67', fromUser: currentUser })
            }
            label={
              submittingOrder?.type == 'Order67' ? 'Submitting' : 'Order 67'
            }
            disabled={submittingOrder != null}
            loading={submittingOrder?.type == 'Order67'}
          />
        </>
      }
    />
  );
};

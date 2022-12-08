import { Order, WorkflowReportData } from '../../../server/types';
import React from 'react';
import Button from '../system/Button';

interface WorkflowReportCardProps {
  workflowReport: WorkflowReportData;
  onStartAgain: () => {};
  onSendOrder: (order: Order) => {};
  submittingOrder: Order | undefined;
  currentUser: string;
}
export const WorkflowReportCard: React.FC<WorkflowReportCardProps> = ({
  workflowReport,
  onStartAgain,
  onSendOrder,
  submittingOrder,
  currentUser,
}) => {
  return (
    <div className="bg-white px-4 py-3 shadow-lg sm:rounded-md md:px-8 md:py-6">
      <div className="flex flex-row items-center justify-between">
        <h2 className="text-lg md:text-2xl">
          Workflow started: {workflowReport.workflowId}
        </h2>
        <Button
          type="button"
          variant={
            workflowReport.workflowStatus == 'DONE' ? 'primary' : 'tertiary'
          }
          onClick={onStartAgain}
          label="Start Again"
        />
      </div>
      <p>Troopers Danced: {workflowReport.troopersDanced}</p>
      <p>Jedi Eliminated: {workflowReport.jediEliminated}</p>
      <p>Jedi Remaining: {workflowReport.jediRemaining}</p>
      <div className="mt-3 flex flex-row justify-center gap-4">
        {/* TODO - add transition, hide the button not clicked and centre other */}
        <Button
          type="button"
          variant="secondary"
          size="large"
          onClick={() =>
            onSendOrder({ type: 'Order66', fromUser: currentUser })
          }
          label={submittingOrder?.type == 'Order66' ? 'Submitting' : 'Order 66'}
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
          label={submittingOrder?.type == 'Order67' ? 'Submitting' : 'Order 67'}
          disabled={submittingOrder != null}
          loading={submittingOrder?.type == 'Order67'}
        />
      </div>
    </div>
  );
};

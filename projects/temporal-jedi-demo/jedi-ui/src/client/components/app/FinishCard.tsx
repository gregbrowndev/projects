import { WorkflowReportData } from '../../../server/types';
import React from 'react';
import Button from '../system/Button';
import { Card } from '../system/Card';
import { StatsList } from './StatsList';
import Image, { StaticImageData } from 'next/image';

export interface FinishCardProps {
  workflowReport: WorkflowReportData;
  onStartAgain: () => {};
  startingAgain: boolean;
  image: StaticImageData;
}
export const FinishCard: React.FC<FinishCardProps> = ({
  workflowReport,
  onStartAgain,
  startingAgain = false,
  image,
}) => {
  return (
    <Card
      title="Workflow Complete"
      body={
        <>
          <Image src={image} alt="" className="h-96 w-full object-contain" />
          <StatsList
            items={
              <>
                <li>
                  You danced with {workflowReport.troopersDanced} Storm Troopers
                </li>
                <li>
                  You eliminated {workflowReport.jediEliminated} Jedi traitors
                </li>
              </>
            }
          />
        </>
      }
      footer={
        <Button
          type="button"
          variant="primary"
          size="large"
          onClick={onStartAgain}
          label={startingAgain ? 'Processing...' : 'Start Again'}
          disabled={startingAgain}
          loading={startingAgain}
        />
      }
    />
  );
};

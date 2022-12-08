import Button from '../system/Button';
import React from 'react';

export interface StartCardProps {
  onStartHandler: () => void;
  starting: boolean;
}

export const StartCard: React.FC<StartCardProps> = ({
  onStartHandler,
  starting,
}) => {
  return (
    <div className="bg-white px-4 py-3 shadow-lg sm:rounded-md md:px-8 md:py-6">
      <h2 className="text-2xl">Workflow not started</h2>
      <div className="mt-3 flex flex-row justify-center gap-4">
        <Button
          type="button"
          size="large"
          variant="primary"
          onClick={onStartHandler}
          disabled={starting}
          loading={starting}
          label={starting ? 'Starting' : 'Start'}
        />
      </div>
    </div>
  );
};

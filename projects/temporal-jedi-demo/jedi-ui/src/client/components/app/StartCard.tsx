import Button from '../system/Button';
import React from 'react';
import { Card } from '../system/Card';
import Image, { StaticImageData } from 'next/image';

export interface StartCardProps {
  onStartHandler: () => void;
  starting: boolean;
  image: StaticImageData;
}

export const StartCard: React.FC<StartCardProps> = ({
  onStartHandler,
  starting,
  image,
}) => {
  return (
    <Card
      title="Lets Go"
      body={<Image src={image} alt="" className="h-96 w-full object-contain" />}
      footer={
        <Button
          type="button"
          size="large"
          variant="primary"
          onClick={onStartHandler}
          disabled={starting}
          loading={starting}
          label={starting ? 'Starting' : 'Start'}
        />
      }
    />
  );
};

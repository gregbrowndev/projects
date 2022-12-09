import { OrderReportData } from '../../../server/types';
import Image, { StaticImageData } from 'next/image';
import React from 'react';
import { Transition } from '@headlessui/react';
import Button from '../system/Button';
import { Card } from '../system/Card';
import { StatsList } from './StatsList';

export interface OrderReportCardProps {
  orderReport: OrderReportData;
  image: StaticImageData;
  onDoneHandler: () => void;
}
export const OrderReportCard: React.FC<OrderReportCardProps> = ({
  orderReport,
  image,
  onDoneHandler,
}) => {
  const orderTitle = orderReport.type == 'Order66' ? 'Order 66' : 'Order 67';
  return (
    <Card
      title={
        orderReport.status == 'EXECUTING'
          ? 'Meanwhile...'
          : `${orderTitle} Complete`
      }
      body={
        <div className="flex flex-col items-center">
          {/* Executing Section */}
          <Transition
            appear={true}
            show={orderReport.status == 'EXECUTING'}
            enter="transition-opacity duration-300"
            enterFrom="opacity-0"
            enterTo="opacity-100"
            leave="transition-opacity duration-300"
            leaveFrom="opacity-100"
            leaveTo="opacity-0"
            className="h-50 md:h-96"
          >
            <Image
              src={image}
              alt="Dancing storm trooper"
              className="h-full w-full object-contain"
              priority
            />
          </Transition>

          {/* Results Section */}
          <Transition
            show={orderReport.status != 'EXECUTING'}
            enter="transition-opacity duration-300"
            enterFrom="opacity-0"
            enterTo="opacity-100"
            leave="transition-opacity duration-300"
            leaveFrom="opacity-100"
            leaveTo="opacity-0"
            className="flex w-full flex-col items-center"
          >
            <div className="flex w-full grow flex-row items-center justify-between">
              {orderReport.type == 'Order66' && (
                <>
                  <span className="ml-4 text-5xl md:ml-16 md:text-7xl">☠️</span>
                  <div className="grow">
                    <StatsList
                      items={
                        <>
                          <li>
                            Jedi Eliminated: {orderReport.jediEliminated || 0}
                          </li>
                          <li>
                            Jedi Remaining:{' '}
                            {orderReport.totalJediRemaining || 0}
                          </li>
                        </>
                      }
                    />
                  </div>
                  <span className="mr-4 text-5xl md:mr-16 md:text-7xl">☠️</span>
                </>
              )}
              {orderReport.type == 'Order67' && (
                <>
                  <span className="ml-4 text-5xl md:ml-16 md:text-7xl">💃</span>
                  <div className="grow">
                    <StatsList
                      items={
                        <>
                          <li>
                            Storm Troopers Danced:{' '}
                            {orderReport.troopersDanced || 0}
                          </li>
                          <li>
                            Total Danced: {orderReport.totalTroopersDanced || 0}
                          </li>
                        </>
                      }
                    />
                  </div>
                  <span className="mr-4 text-5xl md:mr-16 md:text-7xl">🕺</span>
                </>
              )}
            </div>
          </Transition>
        </div>
      }
      footer={
        <Button
          type="button"
          variant="primary"
          size="large"
          label={orderReport.status == 'EXECUTING' ? 'Executing...' : 'Done'}
          onClick={onDoneHandler}
          loading={orderReport.status == 'EXECUTING'}
          disabled={orderReport.status == 'EXECUTING'}
        />
      }
    />
  );
};

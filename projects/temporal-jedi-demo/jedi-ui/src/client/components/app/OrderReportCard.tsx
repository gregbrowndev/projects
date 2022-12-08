import { OrderReportData } from '../../../server/types';
import Image, { StaticImageData } from 'next/image';
import React from 'react';
import { Transition } from '@headlessui/react';
import Button from '../system/Button';

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
  return (
    <div className="bg-white px-4 py-3 shadow-lg sm:rounded-md md:px-8 md:py-6">
      {/* Inner content */}
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
          className="h-96"
        >
          <Image
            src={image}
            alt="Dancing storm trooper"
            className="h-full w-full object-contain"
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
          className="flex h-64 w-full flex-col items-center"
        >
          <h2 className="mt-1 text-center text-2xl md:mt-8 md:text-4xl">
            Order Complete!
          </h2>
          <div className="flex w-full grow flex-row items-center justify-between">
            {orderReport.type == 'Order66' && (
              <>
                <span className="ml-16 text-7xl">☠️</span>
                <div className="grow">
                  <ul className="mt-1 text-center text-xl md:mt-3 md:text-3xl">
                    <li>Jedi Eliminated: {orderReport.jediEliminated || 0}</li>
                    <li>
                      Jedi Remaining: {orderReport.totalJediRemaining || 0}
                    </li>
                  </ul>
                </div>
                <span className="mr-16 text-7xl">☠️</span>
              </>
            )}
            {orderReport.type == 'Order67' && (
              <>
                <span className="ml-16 text-7xl">💃</span>
                <div className="grow">
                  <ul className="mt-1 text-center text-xl md:mt-3 md:text-3xl">
                    <li>
                      Storm Troopers Danced: {orderReport.troopersDanced || 0}
                    </li>
                    <li>
                      Total Danced: {orderReport.totalTroopersDanced || 0}
                    </li>
                  </ul>
                </div>
                <span className="mr-16 text-7xl">🕺</span>
              </>
            )}
          </div>
        </Transition>

        {/* Continue Button */}
        <div className="mt-4">
          <Button
            type="button"
            variant="primary"
            size="large"
            label={orderReport.status == 'EXECUTING' ? 'Executing...' : 'Done'}
            onClick={onDoneHandler}
            loading={orderReport.status == 'EXECUTING'}
            disabled={orderReport.status == 'EXECUTING'}
          />
        </div>
      </div>
    </div>
  );
};

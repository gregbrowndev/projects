import React, { useCallback, useEffect, useMemo, useState } from 'react';
import { GetServerSideProps, NextPage } from 'next';
import Head from 'next/head';
import { useInterval } from '../hooks/useInterval';
import Image from 'next/image';

import dancingGif1 from '../../public/static/gifs/storm-trooper-star-wars.gif';
import dancingGif2 from '../../public/static/gifs/dancing-stormtrooper.gif';
import dancingGif3 from '../../public/static/gifs/star-wars-dance-bgt.gif';
import dancingGif4 from '../../public/static/gifs/darth-vader-dance-star-wars.gif';
import dancingGif5 from '../../public/static/gifs/ballroom-dance.gif';
import dancingGif6 from '../../public/static/gifs/meme-star-wars.gif';
import dancingGif7 from '../../public/static/gifs/sassy-dance.gif';

import Button from '../components/Button';
import { Switch, Transition } from '@headlessui/react';
import { OrderReportData } from '../server/types';
import { getOrderReport } from '../server/queries';
import { getWorkflowId } from '../server/utils';

const REFRESH_INTERVAL_MS = 1000;

function randomInt(min: number, max: number) {
  return Math.floor(Math.random() * (max - min + 1) + min);
}

interface Props {
  workflowId: string;
  orderReport: OrderReportData;
}
export const getServerSideProps: GetServerSideProps<Props> = async ({
  req,
  res,
}) => {
  const workflowId = getWorkflowId({ req, res });
  if (!workflowId) {
    throw new Error('Workflow not started');
  }

  const orderReport = await getOrderReport(workflowId);
  if (!orderReport) {
    // TODO - navigate to home page
    throw new Error('Order not started');
  }

  return {
    props: {
      workflowId,
      orderReport,
    },
  };
};

const OrderStatusPage: NextPage<Props> = (props) => {
  // Initial SS props contains orderReport that shows which order is executing

  // DEBUGGING
  const [isExecuting, setIsExecuting] = useState(true);

  /* Set up useInterval to poll getOrderStatus */
  const [orderReport, setOrderReport] = useState<OrderReportData>(
    props.orderReport,
  );
  const fetchOrderReport = useCallback(async () => {
    console.log('fetchOrderStatus called');
    // TODO - replace with real code, would be good to mock this out
    // const orderStatus = await getOrderStatus().then((res) => {
    //   if (!isErrorData(res)) {
    //     return res.orderStatus;
    //   }
    // });
    const refreshedOrderReport: OrderReportData = {
      ...props.orderReport,
      status: isExecuting ? 'EXECUTING' : 'WAITING',
    };
    setOrderReport(refreshedOrderReport);
  }, [isExecuting]);

  useInterval(
    () => {
      if (orderReport.status == 'EXECUTING') {
        fetchOrderReport().catch(console.error);
      }
    },
    REFRESH_INTERVAL_MS,
    [REFRESH_INTERVAL_MS, fetchOrderReport, orderReport, isExecuting],
  );

  const dancingGifs = useMemo(
    () => [
      dancingGif1,
      dancingGif2,
      dancingGif3,
      dancingGif4,
      dancingGif5,
      dancingGif6,
      dancingGif7,
    ],
    [],
  );

  const [selectedGifId, setSelectedGifId] = useState<number | null>(null);
  useEffect(() => {
    setSelectedGifId(randomInt(0, dancingGifs.length - 1));
  }, [dancingGifs]);
  // TODO - it would be better to cycle through them to avoid showing the same one. Need to store some state globally?

  const toggle = (
    <div className="align-items flex flex-row">
      <Switch
        checked={isExecuting}
        onChange={() => setIsExecuting(!isExecuting)}
        className={`${
          isExecuting ? 'bg-blue-600' : 'bg-gray-200'
        } relative inline-flex h-6 w-11 items-center rounded-full`}
      >
        <span className="sr-only">Toggle Executing State</span>
        <span
          className={`${
            isExecuting ? 'translate-x-6' : 'translate-x-1'
          } inline-block h-4 w-4 transform rounded-full bg-white transition`}
        />
      </Switch>
      <span className="ml-2">Toggle Executing State</span>
    </div>
  );

  return (
    <>
      <Head>
        <title>Jedi Demo</title>
        <link rel="icon" href="/static/temporal.ico" />
      </Head>

      <section>
        <h1 className="text-2xl font-bold md:text-6xl">
          Executing <span className="text-blue-600">Order 67</span>
        </h1>

        <p className="mt-1 text-lg md:mt-3 md:text-2xl">
          Deploying elite dance troopers
        </p>
      </section>

      <section className="mt-6 md:mt-16">
        <div className="bg-white px-4 py-3 shadow-lg sm:rounded-md md:px-8 md:py-6">
          {/* Inner content */}
          <div className="flex flex-col items-center">
            <div className="h-96">
              <Transition
                appear={true}
                show={
                  orderReport.status == 'EXECUTING' && selectedGifId != null
                }
                enter="transition-opacity duration-150"
                enterFrom="opacity-0"
                enterTo="opacity-100"
                leave="transition-opacity duration-300"
                leaveFrom="opacity-100"
                leaveTo="opacity-0"
                className="h-full"
              >
                {selectedGifId != null && (
                  <Image
                    src={dancingGifs[selectedGifId]}
                    alt="Dancing storm trooper"
                    className="h-full w-full object-contain"
                  />
                )}
              </Transition>
              {/* TODO - add results section */}
            </div>
            <div className="mt-4">
              <Button
                type="button"
                variant="primary"
                label={
                  orderReport.status == 'EXECUTING'
                    ? 'Executing...'
                    : 'Send another'
                }
                loading={orderReport.status == 'EXECUTING'}
                disabled={orderReport.status == 'EXECUTING'}
              />
            </div>
            <div className="mt-8">{toggle}</div>
            <div className="mt-8">{orderReport.status}</div>
          </div>
        </div>
      </section>
    </>
  );
};

export default OrderStatusPage;

import React, { useCallback, useMemo, useState } from 'react';
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
import { Transition } from '@headlessui/react';
import { isErrorData, OrderReportData } from '../server/types';
import { getWorkflowId } from '../server/utils';
import { useRouter } from 'next/router';

import * as client from '../client/httpClient';
import * as server from '../server/queries';

const REFRESH_INTERVAL_MS = 2000;

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
    return {
      redirect: {
        destination: '/',
        permanent: false,
      },
    };
  }

  const orderReport = await server.getOrderReport(workflowId);
  if (!orderReport) {
    console.error('Order not started');
    return {
      redirect: {
        destination: '/workflow',
        permanent: false,
      },
    };
  }

  return {
    props: {
      workflowId,
      orderReport,
    },
  };
};

const OrderStatusPage: NextPage<Props> = (props) => {
  const router = useRouter();

  /* Set up useInterval to poll getOrderReport */
  const [orderReport, setOrderReport] = useState<OrderReportData>(
    props.orderReport,
  );
  const fetchOrderReport = useCallback(async () => {
    console.log('fetchOrderReport called');
    const response = await client.getOrderReport();

    if (isErrorData(response)) {
      // TODO - add toaster for error message
      console.error(response);
      return;
    }

    console.log('received orderReport', response);
    setOrderReport(response);
  }, []);

  useInterval(
    () => {
      if (orderReport.status == 'EXECUTING') {
        fetchOrderReport().catch(console.error);
      }
    },
    REFRESH_INTERVAL_MS,
    [REFRESH_INTERVAL_MS, fetchOrderReport, orderReport],
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

  const selectedGifId = useMemo(() => {
    // TODO - it would be better to cycle through them to avoid showing the same one. Need to store some state globally?
    return randomInt(0, dancingGifs.length - 1);
  }, [dancingGifs.length]);

  const onDoneHandler = async () => {
    await router.push('/workflow');
  };

  return (
    <>
      <Head>
        <title>Jedi Demo</title>
        <link rel="icon" href="/static/temporal.ico" />
      </Head>

      <section>
        <h1 className="text-2xl font-bold md:text-6xl">
          Executing{' '}
          <span className="text-blue-600">
            {orderReport.type == 'Order66' ? 'Order 66' : 'Order 67'}
          </span>
        </h1>

        <p className="mt-1 text-lg md:mt-3 md:text-2xl">
          {orderReport.type == 'Order66'
            ? 'Eliminate all Jedi'
            : 'Deploying elite dance troopers'}
        </p>
      </section>

      <section className="mt-6 md:mt-16">
        <div className="bg-white px-4 py-3 shadow-lg sm:rounded-md md:px-8 md:py-6">
          {/* Inner content */}
          <div className="flex flex-col items-center">
            {/* Executing Section */}
            <Transition
              appear={true}
              show={orderReport.status == 'EXECUTING' && selectedGifId != null}
              enter="transition-opacity duration-300"
              enterFrom="opacity-0"
              enterTo="opacity-100"
              leave="transition-opacity duration-300"
              leaveFrom="opacity-100"
              leaveTo="opacity-0"
              className="h-96"
            >
              {selectedGifId != null && (
                <Image
                  src={dancingGifs[selectedGifId]}
                  alt="Dancing storm trooper"
                  className="h-full w-full object-contain"
                />
              )}
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
                <span className="ml-16 text-7xl">💃</span>
                <div className="grow">
                  <h3 className="mt-1 text-center text-xl md:mt-3 md:text-3xl">
                    Storm Troopers Danced: {orderReport.troopersDanced || 0}
                  </h3>
                </div>
                <span className="mr-16 text-7xl">🕺</span>
              </div>
            </Transition>

            {/* Continue Button */}
            <div className="mt-4">
              <Button
                type="button"
                variant="primary"
                size="large"
                label={
                  orderReport.status == 'EXECUTING' ? 'Executing...' : 'Done'
                }
                onClick={onDoneHandler}
                loading={orderReport.status == 'EXECUTING'}
                disabled={orderReport.status == 'EXECUTING'}
              />
            </div>
          </div>
        </div>
      </section>
    </>
  );
};

export default OrderStatusPage;

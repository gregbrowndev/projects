import React, { useCallback, useEffect, useState } from 'react';
import { NextPage } from 'next';
import Head from 'next/head';
import { getOrderStatus, getWorkflowIdCookie } from '../services/DataService';
import { isErrorData, OrderStatus } from '../services/hack';
import { useInterval } from '../hooks/useInterval';
import Image from 'next/image';

import dancingGif1 from '../../public/static/gifs/storm-trooper-star-wars.gif';
import dancingGif2 from '../../public/static/gifs/dancing-stormtrooper.gif';
import Button from '../components/Button';

const REFRESH_INTERVAL_MS = 1000;

function randomInt(min: number, max: number) {
  return Math.floor(Math.random() * (max - min + 1) + min);
}

const OrderStatusPage: NextPage = () => {
  /* Get workflowId from cookie */
  const [workflowId, setWorkflowId] = useState<string | undefined>(undefined);
  useEffect(() => {
    const cookieVal = getWorkflowIdCookie();
    setWorkflowId(cookieVal);
    if (!cookieVal) {
      // TODO - navigate to root
    }
  }, []);

  /* Set up useInterval to poll getOrderStatus */
  // TODO - need to pass across orderType too
  const [orderStatus, setOrderStatus] = useState<OrderStatus>('EXECUTING');
  const fetchOrderStatus = useCallback(async () => {
    console.log('fetchOrderStatus called');
    const orderStatus = await getOrderStatus().then((res) => {
      if (!isErrorData(res)) {
        return res.orderStatus;
      }
    });
    +setOrderStatus(orderStatus || 'WAITING');
  }, []);

  useInterval(
    () => {
      if (!workflowId) {
        return;
      }
      fetchOrderStatus().catch(console.error);
    },
    REFRESH_INTERVAL_MS,
    [REFRESH_INTERVAL_MS, workflowId, fetchOrderStatus],
  );

  const dancingGifs = [dancingGif1, dancingGif2];

  // TODO - it would be better to cycle through them to avoid showing the same one. Need to store some state globally?
  const selectedGif = dancingGifs[randomInt(0, dancingGifs.length - 1)];

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
            <div>
              <Image
                src={selectedGif}
                alt="Dancing storm trooper"
                className="h-96 w-full object-contain"
              />
            </div>
            <div className="mt-4">
              <Button
                type="button"
                variant="primary"
                label={
                  orderStatus == 'EXECUTING' ? 'Executing...' : 'Order again'
                }
                loading={orderStatus == 'EXECUTING'}
                disabled={orderStatus == 'EXECUTING'}
              />
            </div>
          </div>
        </div>
      </section>
    </>
  );
};

export default OrderStatusPage;

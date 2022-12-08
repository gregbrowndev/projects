import React, { useCallback, useMemo, useState } from 'react';
import { GetServerSideProps, NextPage } from 'next';
import Head from 'next/head';
import { useInterval } from '../client/hooks/useInterval';

import { isErrorData, OrderReportData } from '../server/types';
import { useRouter } from 'next/router';

import * as client from '../client/http/client';
import * as server from '../server/queries';
import { OrderReportCard } from '../client/components/app/OrderReportCard';
import { getWorkflowId } from '../server/cookies';
import { randomInt } from '../client/utils';
import { ORDER_66_GIFS, ORDER_67_GIFS } from '../client/gifs';

const REFRESH_INTERVAL_MS = 2000;

const OrderReportPage: NextPage<ServerProps> = (props) => {
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

  const order66Gifs = useMemo(() => ORDER_66_GIFS, []);
  const order67Gifs = useMemo(() => ORDER_67_GIFS, []);

  const orderGifs = orderReport.type == 'Order66' ? order66Gifs : order67Gifs;
  const selectedGif = useMemo(() => {
    // TODO - it would be better to cycle through them to avoid showing the same one. Need to store some state globally?
    const index = randomInt(0, orderGifs.length - 1);
    return orderGifs[index];
  }, [orderGifs]);

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
            ? 'Eliminating Jedi traitors'
            : 'Deploying elite dance troopers'}
        </p>
      </section>

      <section className="mt-6 md:mt-16">
        {!!orderReport && !!selectedGif && (
          <OrderReportCard
            orderReport={orderReport}
            image={selectedGif}
            onDoneHandler={onDoneHandler}
          />
        )}
      </section>
    </>
  );
};

interface ServerProps {
  workflowId: string;
  orderReport: OrderReportData;
}

export const getServerSideProps: GetServerSideProps<ServerProps> = async ({
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

export default OrderReportPage;

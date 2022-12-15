/**
 * @jest-environment node
 */

import { TestWorkflowEnvironment } from '@temporalio/testing';
import { DefaultLogger, LogEntry, Runtime, Worker } from '@temporalio/worker';
import { Client } from '@temporalio/client';

import type * as activities from '../activities'; // Uses types to ensure our mock signatures match
import {
  jediBusiness,
  orderReportQuery,
  orderSignal,
  workflowReportQuery,
} from '../workflows';
import { uuid4 } from '@temporalio/workflow';
import { OrderReport, WorkflowReport } from '../types';

// See full example at https://github.com/vkarpov15/temporal-ecommerce-ts/blob/95b7be70d0b2a8d95cea93ec719453c94fa68f2e/src/test/workflows.test.ts#L65-L80

// creating TestWorkflowEnvironment can cause test to time out
// the first time it is executed as it needs to download a Java binary
// Note, if tests get stuck check/delete the file at /tmp/temporalite-sdk-typescript-1.4.4.downloading
jest.setTimeout(20_000);

describe('Workflow', () => {
  let env: TestWorkflowEnvironment;
  let worker: Worker;
  let runPromise: Promise<void>;
  let client: Client;
  let ordersReceived: string[];
  const mockActivities: Partial<typeof activities> = {
    executeOrder: async (id) => {
      ordersReceived.push(id);
      return id;
    },
  };

  beforeAll(async () => {
    // Use console.log instead of console.error to avoid red output
    // Filter INFO log messages for clearer test output
    Runtime.install({
      logger: new DefaultLogger('WARN', (entry: LogEntry) =>
        console.log(`[${entry.level}]`, entry.message),
      ),
    });

    env = await TestWorkflowEnvironment.createTimeSkipping();
    client = env.client;
    worker = await Worker.create({
      connection: env.nativeConnection,
      taskQueue: 'test',
      workflowsPath: require.resolve('../workflows'),
      activities: mockActivities,
    });
    runPromise = worker.run();
  });

  afterAll(async () => {
    worker?.shutdown();
    await runPromise;
    await env?.teardown();
  });

  beforeEach(() => {
    ordersReceived = [];
  });

  it('handles order 67 and troopers dance', async () => {
    // GIVEN
    const handle = await client.workflow.start(jediBusiness, {
      workflowId: uuid4(),
      taskQueue: 'test',
    });

    // It should have an initial workflowReport with WAITING state
    let workflowReport = await handle.query(workflowReportQuery);
    expect(workflowReport).toEqual({
      workflowStatus: 'WAITING',
      currentOrderStatus: undefined,
      troopersDanced: 0,
      jediEliminated: 0,
      jediRemaining: 10,
    } as WorkflowReport);

    // AND no orders have been received
    expect(ordersReceived).toStrictEqual([]);

    // WHEN
    await handle.signal(orderSignal, {
      type: 'Order67',
      fromUser: 'Darth Sidious',
    });

    // THEN
    workflowReport = await handle.query(workflowReportQuery);
    expect(workflowReport).toEqual({
      workflowStatus: 'EXECUTING',
      currentOrderStatus: 'EXECUTING',
      troopersDanced: 0,
      jediEliminated: 0,
      jediRemaining: 10,
    } as WorkflowReport);

    let orderReport = await handle.query(orderReportQuery);
    expect(orderReport).toEqual({
      type: 'Order67',
      status: 'EXECUTING',
      troopersDanced: undefined,
      jediEliminated: undefined,
    } as OrderReport);

    expect(ordersReceived).toStrictEqual(['Order67']);

    // WHEN
    await env.sleep(10000);

    // THEN
    workflowReport = await handle.query(workflowReportQuery);
    expect(workflowReport).toEqual({
      workflowStatus: 'WAITING',
      currentOrderStatus: 'DONE',
      troopersDanced: 5,
      jediEliminated: 0,
      jediRemaining: 10,
    } as WorkflowReport);

    orderReport = await handle.query(orderReportQuery);
    expect(orderReport).toEqual({
      type: 'Order67',
      status: 'DONE',
      troopersDanced: 5,
      jediEliminated: undefined,
    } as OrderReport);

    await handle.terminate(); // terminate workflow
  });

  it('completes when it receives order 66', async () => {
    // GIVEN
    const handle = await client.workflow.start(jediBusiness, {
      workflowId: uuid4(),
      taskQueue: 'test',
    });

    // WHEN
    await handle.signal(orderSignal, {
      type: 'Order66',
      fromUser: 'Darth Sidious',
    });

    // THEN
    let orderReport = await handle.query(orderReportQuery);
    expect(orderReport).toEqual({
      type: 'Order66',
      status: 'EXECUTING',
      troopersDanced: undefined,
      jediEliminated: undefined,
    } as OrderReport);

    // skip time 10s
    await env.sleep(10000);

    orderReport = await handle.query(orderReportQuery);
    expect(orderReport).toEqual({
      type: 'Order66',
      status: 'DONE',
      troopersDanced: undefined,
      jediEliminated: 5,
    } as OrderReport);

    let workflowReport = await handle.query(workflowReportQuery);
    expect(workflowReport).toEqual({
      workflowStatus: 'DONE',
      currentOrderStatus: 'DONE',
      troopersDanced: 0,
      jediEliminated: 5,
      jediRemaining: 5,
    } as WorkflowReport);

    expect(ordersReceived).toStrictEqual(['Order66']);

    const result = await handle.result();
    expect(result).toBeUndefined(); // check the workflow completes
  });
});

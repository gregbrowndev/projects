/**
 * @jest-environment node
 */

import { TestWorkflowEnvironment } from '@temporalio/testing';
import { DefaultLogger, Runtime, LogEntry, Worker } from '@temporalio/worker';
import { Client } from '@temporalio/client';

import type * as activities from '../activities'; // Uses types to ensure our mock signatures match
import {jediBusiness, orderSignal, orderStatusQuery, teaDrunkQuery} from '../workflows';
import { uuid4 } from '@temporalio/workflow';

// See full example at https://github.com/vkarpov15/temporal-ecommerce-ts/blob/95b7be70d0b2a8d95cea93ec719453c94fa68f2e/src/test/workflows.test.ts#L65-L80

// creating TestWorkflowEnvironment can cause test to time out
// the first time it is executed as it needs to download a Java binary
// Note, if tests get stuck check/delete the file at /tmp/temporalite-sdk-typescript-1.4.4.downloading
jest.setTimeout(20_000)


describe('Workflow', () => {
  let env: TestWorkflowEnvironment;
  let worker: Worker;
  let runPromise: Promise<void>;
  let client: Client;

  let ordersReceived: string[]
  const mockActivities: Partial<typeof activities> = {
    executeOrder: async (id) => {
      ordersReceived.push(id)
      return id
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
    runPromise = worker.run()
  });

  afterAll(async () => {
    worker?.shutdown()
    await runPromise
    await env?.teardown();
  });

  beforeEach(() => {
    ordersReceived = []
  })

  it('handles order 67 and drinks tea', async () => {
    const handle = await client.workflow.start(jediBusiness, {
      workflowId: uuid4(),
      taskQueue: 'test',
    });

    // Its initial state should be WAITING
    let orderStatus = await handle.query(orderStatusQuery);
    expect(orderStatus).toEqual('WAITING');

    let teaDrunk = await handle.query(teaDrunkQuery);
    expect(teaDrunk).toEqual(0);

    expect(ordersReceived).toStrictEqual([])

    await handle.signal(orderSignal, {type: 'Order67', fromUser: 'Darth Sidious'})

    orderStatus = await handle.query(orderStatusQuery);
    expect(orderStatus).toEqual('EXECUTING');

    teaDrunk = await handle.query(teaDrunkQuery);
    expect(teaDrunk).toEqual(1);

    expect(ordersReceived).toStrictEqual(["Order67"])

    await handle.terminate()  // terminate dangling workflow
  });

  it('completes when it receives order 66', async () => {
    const handle = await client.workflow.start(jediBusiness, {
      workflowId: uuid4(),
      taskQueue: 'test',
    });

    await handle.signal(orderSignal, {type: 'Order66', fromUser: 'Darth Sidious'})

    let orderStatus = await handle.query(orderStatusQuery);
    expect(orderStatus).toEqual('EXECUTING');

    let teaDrunk = await handle.query(teaDrunkQuery);
    expect(teaDrunk).toEqual(0);

    // skip time 10s
    await env.sleep(10000)

    orderStatus = await handle.query(orderStatusQuery);
    expect(orderStatus).toEqual('DONE');

    expect(ordersReceived).toStrictEqual(["Order66"])

    const result = await handle.result()
    expect(result).toBeUndefined()  // check the workflow completes
  })
});

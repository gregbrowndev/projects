import { MockActivityEnvironment } from '@temporalio/testing';
import * as activities from '../activities'; // purely for type safety

describe('Activity', () => {
  let env: MockActivityEnvironment;

  beforeAll(() => {
    env = new MockActivityEnvironment({ attempt: 2 });
  });

  describe('executeOrder', () => {
    it('returns the order ID', async () => {
      // some stuff
      const result = await env.run(activities.executeOrder, '66');
      expect(result).toBe('66');
    });
  });
});

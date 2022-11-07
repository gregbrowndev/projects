import { MockActivityEnvironment } from '@temporalio/testing';
import * as activities from '../activities'; // purely for type safety

describe('Activity', () => {
  let env: MockActivityEnvironment;

  beforeAll(() => {
    env = new MockActivityEnvironment({ attempt: 2 });
  });

  describe('purchase', () => {

    it('returns the activity ID', async () => {
      // some stuff
      const result = await env.run(activities.purchase, '2');
      expect(result).toBe("test")
    });
  });
});

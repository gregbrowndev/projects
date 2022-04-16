import request from 'supertest';
import { makeApp } from '../app';
import { Express } from 'express';

describe('api/current-user', () => {
  let app: Express;

  beforeEach(async () => {
    process.env.JWT_KEY = 'abc';
    app = await makeApp();
  });

  it('responds with details about the current user', async () => {
    const cookie = await global.signin();
    const response = await request(app)
      .get('/api/users/currentuser')
      .set('Cookie', cookie)
      .send()
      .expect(200);

    expect(response.body.currentUser.email).toEqual('test@test.com');
  });

  it('responds with null if not authenticated', async () => {
    const response = await request(app)
      .get('/api/users/currentuser')
      .send()
      .expect(200);

    expect(response.body.currentUser).toEqual(null);
  });
});

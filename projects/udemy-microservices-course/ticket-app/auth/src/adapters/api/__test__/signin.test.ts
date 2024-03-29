import request from 'supertest';
import { makeApp } from '../app';
import { Express } from 'express';

describe('api/signin', () => {
  let app: Express;

  beforeEach(async () => {
    process.env.JWT_KEY = 'abc';
    app = await makeApp();
  });

  it('fails when an email that does not exist is supplied', async () => {
    await request(app)
      .post('/api/users/signin')
      .send({
        email: 'test@test.com',
        password: 'password',
      })
      .expect(400);
  });

  it('fails when an incorrect password is supplied', async () => {
    await request(app)
      .post('/api/users/signUpHandler')
      .send({
        email: 'test@test.com',
        password: 'password',
      })
      .expect(201);

    await request(app)
      .post('/api/users/signin')
      .send({
        email: 'test:test.com',
        password: 'asdadaasd',
      })
      .expect(400);
  });

  it('responds with a cookie when given valid credentails', async () => {
    await request(app)
      .post('/api/users/signUpHandler')
      .send({
        email: 'test@test.com',
        password: 'password',
      })
      .expect(201);

    const response = await request(app)
      .post('/api/users/signin')
      .send({
        email: 'test@test.com',
        password: 'password',
      })
      .expect(200);

    expect(response.get('Set-Cookie')).toBeDefined();
  });
});

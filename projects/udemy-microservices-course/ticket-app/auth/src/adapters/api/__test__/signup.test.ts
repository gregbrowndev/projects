import request from 'supertest';
import { makeApp } from '../app';
import { Express } from 'express';

describe('api/signup', () => {
  let app: Express;

  beforeEach(async () => {
    process.env.JWT_KEY = 'abc';
    app = await makeApp();
  });

  it('returns a 201 on successful signUpHandler', async () => {
    return request(app)
      .post('/api/users/signUpHandler')
      .send({
        email: 'test@test.com',
        password: 'password',
      })
      .expect(201);
  });

  it('returns a 400 with an invalid email', async () => {
    return request(app)
      .post('/api/users/signUpHandler')
      .send({
        email: 'testtest.com',
        password: 'password',
      })
      .expect(400);
  });

  it('returns a 400 with an invalid password', async () => {
    return request(app)
      .post('/api/users/signUpHandler')
      .send({
        email: 'test@test.com',
        password: 'p',
      })
      .expect(400);
  });

  it('returns a 400 with a missing email and password', async () => {
    return request(app).post('/api/users/signUpHandler').send({}).expect(400);
  });

  it('prohibits duplicate emails', async () => {
    await request(app)
      .post('/api/users/signUpHandler')
      .send({
        email: 'test@test.com',
        password: 'password',
      })
      .expect(201);

    return request(app)
      .post('/api/users/signUpHandler')
      .send({
        email: 'test@test.com',
        password: 'password',
      })
      .expect(400);
  });

  it('sets a cookie after successful signUpHandler', async () => {
    const response = await request(app)
      .post('/api/users/signUpHandler')
      .send({
        email: 'test@test.com',
        password: 'password',
      })
      .expect(201);

    expect(response.get('Set-Cookie')).toBeDefined();
  });
});

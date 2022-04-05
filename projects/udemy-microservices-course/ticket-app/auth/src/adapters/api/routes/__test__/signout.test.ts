import request from 'supertest';
import { makeApp } from '../../app';

describe('api/signout', async () => {
  const app = await makeApp();

  it('clears the cookie after signing out', async () => {
    await request(app)
      .post('/api/users/signUpHandler')
      .send({
        email: 'test@test.com',
        password: 'password',
      })
      .expect(201);

    const response = await request(app)
      .post('/api/users/signout')
      .send({})
      .expect(200);

    console.log(response.get('Set-Cookie'));
    expect(response.get('Set-Cookie')[0]).toEqual(
      'express:sess=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT; httponly',
    );
  });
});

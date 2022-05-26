import request from 'supertest';
import { makeApp } from '../app';
import { Express } from 'express';
import { pipe } from 'fp-ts/function';
import { SignInPayloadDTO, SignInSuccessDTO } from '../dtos';
import { fold } from 'fp-ts/Either';
import { Errors } from 'io-ts';
import { InvalidParam } from '../errors/bad-request-error';
import { PathReporter } from 'io-ts/lib/PathReporter';

describe('api/current-user', () => {
  let app: Express;

  beforeEach(async () => {
    process.env.JWT_KEY = 'abc';
    app = await makeApp();
  });

  it('testing io-ts', () => {
    pipe(
      SignInPayloadDTO.decode({ email: 'test@test.com' }),
      fold(
        (errors: Errors) => {
          const output = JSON.stringify(errors);
          // const output2 = PathReporter.report(errors);
          console.log(errors);

          // const invalidParams: InvalidParam[] = errors.map((err) => ({
          //   name: '',
          //   reason: '',
          //   value: err.value,
          // }));
        },
        (data) => {},
      ),
    );
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

  it('responds with undefined if not authenticated', async () => {
    const response = await request(app)
      .get('/api/users/currentuser')
      .send()
      .expect(200);

    expect(response.body.currentUser).toEqual(undefined);
  });
});

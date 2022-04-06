import { MongoMemoryReplSet, MongoMemoryServer } from 'mongodb-memory-server';
import mongoose from 'mongoose';
import request from 'supertest';
import { makeApp } from '../adapters/api/app';
import { CoreApp } from '../core/application/ports';
import { bootstrap } from '../core/bootstrap';

declare global {
  var signin: () => Promise<string[]>;
}

let mongod: MongoMemoryReplSet;
let coreApp: CoreApp;

export function getMongoUri(): string {
  if (mongod) {
    return mongod.getUri();
  }
  throw new Error('Mongod is not running');
}

beforeAll(async () => {
  // brute force approach to set environment variables for testing
  process.env.JWT_KEY = 'asdf';

  // TODO - need to replace this with bootstrap function so boostrap is only
  //  called once for each test

  // TODO - shouldn't need to run in-memory server for unit tests
  mongod = await MongoMemoryReplSet.create({ replSet: { count: 1 } });
  const mongoUri = mongod.getUri();

  // coreApp = await bootstrap({
  //   DB_URL: mongoUri,
  //   JWT_KEY: process.env.JWT_KEY,
  // });
});

// beforeEach(async () => {
//   // TODO - This can be used for e2e tests but shouldn't be necessary for unit tests
//   const collections = await mongoose.connection.db.collections();
//
//   for (let collection of collections) {
//     await collection.deleteMany({});
//   }
// });

afterAll(async () => {
  await mongod.stop();
  // await mongoose.connection.close();
});

global.signin = async () => {
  // TODO - this can be used for e2e tests but shouldn't be necessary for unit tests
  const email = 'test@test.com';
  const password = 'password';

  const app = await makeApp();

  const response = await request(app)
    .post('/api/users/signUpHandler')
    .send({
      email,
      password,
    })
    .expect(201);

  return response.get('Set-Cookie');
};

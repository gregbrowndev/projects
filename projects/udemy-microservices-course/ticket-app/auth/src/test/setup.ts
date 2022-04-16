import { MongoMemoryReplSet } from 'mongodb-memory-server';
import mongoose from 'mongoose';
import request from 'supertest';
import { makeApp } from '../adapters/api/app';
import { createConnection } from '../adapters/mongodb';

declare global {
  var signin: () => Promise<string[]>;
}

let mongod: MongoMemoryReplSet;

export function getMongoUri(): string {
  if (mongod) {
    return mongod.getUri();
  }
  throw new Error('Mongod is not running');
}

beforeAll(async () => {
  // TODO - shouldn't need to run in-memory server for unit tests
  console.log('[setup] Starting InMemory MongoDB');
  mongod = await MongoMemoryReplSet.create({ replSet: { count: 1 } });
  const mongoUri = mongod.getUri();

  // Create default Mongoose connection so the beforeEach hook can clean down collections
  await createConnection(mongoUri);
  console.log('[setup] Connected to InMemory MongoDB');
});

beforeEach(async () => {
  console.log('[setup] Cleaning down MongoDB collections');
  const collections = await mongoose.connection.db.collections();
  for (let collection of collections) {
    await collection.deleteMany({});
  }
  console.log('[setup] Done');
});

afterAll(async () => {
  console.log('[setup] Cleaning up connection and InMemory MongoDB');
  await mongoose.connection.close();
  await mongod.stop();
  console.log('[setup] Done');
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

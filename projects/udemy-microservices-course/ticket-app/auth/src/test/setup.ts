import { MongoMemoryServer } from 'mongodb-memory-server';
import mongoose from 'mongoose';
import request from 'supertest';
import { makeApp } from '../adapters/api/app';

declare global {
  var signin: () => Promise<string[]>;
}

let mongod: MongoMemoryServer;

beforeAll(async () => {
  // brute force approach to set environment variables for testing
  process.env.JWT_KEY = 'asdf';

  mongod = await MongoMemoryServer.create();
  const mongoUri = await mongod.getUri();

  await mongoose.connect(mongoUri, {
    useNewUrlParser: true,
    useUnifiedTopology: true,
  });
});

beforeEach(async () => {
  const collections = await mongoose.connection.db.collections();

  for (let collection of collections) {
    await collection.deleteMany({});
  }
});

afterAll(async () => {
  await mongod.stop();
  await mongoose.connection.close();
});

global.signin = async () => {
  const email = 'test@test.com';
  const password = 'password';

  const app = makeApp();

  const response = await request(app)
    .post('/api/users/signUpHandler')
    .send({
      email,
      password,
    })
    .expect(201);

  return response.get('Set-Cookie');
};

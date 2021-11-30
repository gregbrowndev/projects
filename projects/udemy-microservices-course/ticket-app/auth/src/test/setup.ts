import { MongoMemoryServer } from 'mongodb-memory-server';
import mongoose from 'mongoose';
import { app } from '../app';

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

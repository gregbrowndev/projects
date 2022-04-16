import { CustomError } from './custom-error';

export class DatabaseConnectionError extends CustomError {
  statusCode = 500;
  reason: string = 'ApiError connecting to database';

  constructor() {
    super('ApiError connecting to database');
    Object.setPrototypeOf(this, DatabaseConnectionError.prototype);
  }

  serialiseErrors() {
    return [{ message: this.reason }];
  }
}

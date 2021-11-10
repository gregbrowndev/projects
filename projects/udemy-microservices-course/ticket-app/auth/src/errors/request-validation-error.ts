import { ValidationError } from 'express-validator';
import { BadRequestError } from './bad-request-error';

export class RequestValidationError extends BadRequestError {
  constructor(public errors: ValidationError[]) {
    super('Invalid request parameters');
    Object.setPrototypeOf(this, RequestValidationError.prototype);
  }

  serialiseErrors() {
    return this.errors.map((error) => {
      return { message: error.msg, field: error.param };
    });
  }
}

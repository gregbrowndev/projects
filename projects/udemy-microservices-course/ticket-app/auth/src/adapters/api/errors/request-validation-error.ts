import { BadRequestError, InvalidParam } from './bad-request-error';

export class RequestValidationError extends BadRequestError {
  constructor(public invalidParams?: InvalidParam[]) {
    super('Invalid request parameters', invalidParams);
    Object.setPrototypeOf(this, RequestValidationError.prototype);
  }
}

import { CustomError } from './custom-error';

export interface InvalidParam {
  name: string;
  reason: string;
  value?: undefined;
}

export class BadRequestError extends CustomError {
  statusCode = 400;

  constructor(public message: string, public invalidParams?: InvalidParam[]) {
    super(message);
    Object.setPrototypeOf(this, BadRequestError.prototype);
  }
}

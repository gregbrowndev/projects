import { CustomError } from './custom-error';

export class NotAuthorisedError extends CustomError {
  statusCode: number = 401;

  constructor() {
    super('Not authorised');

    Object.setPrototypeOf(this, NotAuthorisedError.prototype);
  }

  serialiseErrors(): { message: string; field?: string }[] {
    return [{ message: 'Not authorised' }];
  }
}

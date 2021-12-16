import { NextFunction, Request, Response } from 'express';
import { CustomError } from '../errors/custom-error';
import { BadRequestError, InvalidParam } from '../errors/bad-request-error';
import { RequestValidationError } from '../errors/request-validation-error';

export interface ErrorResponse {
  // RFC 7807 standard error handling (not full adherence)
  // https://datatracker.ietf.org/doc/html/rfc7807#section-3

  title: string;
  detail?: string;
  statusCode: number;
  invalidParams?: InvalidParam[];
}

export const errorHandler = (
  err: Error,
  req: Request,
  res: Response<ErrorResponse>,
  next: NextFunction,
) => {
  console.error(err);
  if (
    err instanceof (BadRequestError || err instanceof RequestValidationError)
  ) {
    return res.status(err.statusCode).send({
      title: err.message,
      statusCode: err.statusCode,
      invalidParams: err.invalidParams,
    });
  } else if (err instanceof CustomError) {
    return res
      .status(err.statusCode)
      .send({ title: err.message, statusCode: err.statusCode });
  } else {
    res.status(500).send({ title: 'Something went wrong!', statusCode: 500 });
  }
};

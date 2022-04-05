import { Request, Response, NextFunction } from 'express';
import { validationResult } from 'express-validator';
import { RequestValidationError } from '../errors/request-validation-error';

export const validateRequest = (
  req: Request,
  res: Response,
  next: NextFunction,
) => {
  const errors = validationResult(req);

  if (!errors.isEmpty()) {
    const invalidParams = errors
      .array()
      .map((err) => ({ name: err.param, reason: err.msg, value: err.value }));
    throw new RequestValidationError(invalidParams);
  }

  next();
};

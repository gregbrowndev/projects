import { RequestHandler } from 'express';
import { Decoder, Errors, ValidationError } from 'io-ts';
import { fold, left } from 'fp-ts/lib/Either';
import { pipe, flow, identity } from 'fp-ts/function';
import { RequestValidationError } from '../errors/request-validation-error';
import { Tree } from 'fp-ts/Tree';
import { DecodeError } from 'io-ts/DecodeError';
import { ParamsDictionary } from 'express-serve-static-core';
import * as D from 'io-ts/lib/Decoder';
import * as E from 'fp-ts/Either';
import { InvalidParam } from '../errors/bad-request-error';
import { PathReporter } from 'io-ts/lib/PathReporter';

function getErrorValues(forest: Array<Tree<string>>): Array<string> {
  return forest.flatMap((x) => {
    return x.forest.length ? [x.value, ...getErrorValues(x.forest)] : [x.value];
  });
}

function formatError(errors: Errors) {
  const invalidParams: InvalidParam[] = errors.map((err) => ({
    name: err.param,
    reason: err.msg,
    value: err.value,
  }));
  return new RequestValidationError(invalidParams);
}

// function formatErrors(e: E.Either<Errors, any>) {
//   return pipe(e, E.mapLeft(formatError));
// }

type Validator = <I, A>(
  decoder: Decoder<I, A>,
) => RequestHandler<ParamsDictionary, any, I>;

export const validator: Validator = (decoder) => (req, res, next) => {
  pipe(
    decoder.decode(req.body),
    E.mapLeft(formatError),
    fold(
      (errors) => errors,
      () => {
        next();
      },
    ),
  );
};

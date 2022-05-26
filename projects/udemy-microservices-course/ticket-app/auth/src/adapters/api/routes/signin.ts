import express, { Request, Response } from 'express';
import { body } from 'express-validator';
import { validateRequest } from '../middlewares/validate-request';
import { CoreApp, SignInCommand } from '../../../core/application/ports';
import { Email } from '../../../core/domain/email';
import { UnhashedPassword } from '../../../core/domain/unhashedPassword';
import { SignInPayloadDTO, SignInSuccessDTO } from '../dtos';

import { fold, left } from 'fp-ts/Either';
import { pipe } from 'fp-ts/function';
import { validator } from '../middlewares/validator';

export function getSignInRouter(coreApp: CoreApp): express.Router {
  const router = express.Router();
  const decoder = SignInPayloadDTO.asDecoder();

  router.post(
    '/api/users/signin',
    // [
    //   body('email').isEmail().withMessage('Email must be valid'),
    //   body('password')
    //     .trim()
    //     .notEmpty()
    //     .withMessage('Password must be provided'),
    // ],
    // validateRequest,
    validator(decoder),
    async (req: Request, res: Response) => {
      console.log('[signin] request received');

      // req.body

      return pipe(
        SignInPayloadDTO.decode(req.body),
        fold(
          (error) => {
            // return 400
          },
          async (data) => {
            const signInCommand: SignInCommand = {
              email: Email.create(data.email),
              password: UnhashedPassword.create(data.password),
            };

            // Call core service
            const userSignedIn = await coreApp.signIn(signInCommand);

            // Store JWT on session
            // TODO - should this be in the core? How do sessions work for other
            //  server libraries, e.g. gRPC, graphQL?
            req.session = {
              jwt: userSignedIn.token.value,
            };

            const dto: SignInSuccessDTO = {
              id: userSignedIn.id.value,
              email: userSignedIn.email.value,
            };

            res.status(200).send(SignInSuccessDTO.encode(dto));
          },
        ),
      );
    },
  );

  return router;
}

import express, { Request, Response } from 'express';
import { body } from 'express-validator';
import { validateRequest } from '../middlewares/validate-request';
import { CoreApp, SignInCommand } from '../../../core/application/ports';
import { Email } from '../../../core/domain/email';
import { UnhashedPassword } from '../../../core/domain/unhashedPassword';

export function getSignInRouter(coreApp: CoreApp): express.Router {
  const router = express.Router();

  router.post(
    '/api/users/signin',
    [
      body('email').isEmail().withMessage('Email must be valid'),
      body('password')
        .trim()
        .notEmpty()
        .withMessage('Password must be provided'),
    ],
    validateRequest,
    async (req: Request, res: Response) => {
      console.log('[signin] request received');

      // Parse input
      const { email, password } = req.body;
      const signInCommand: SignInCommand = {
        email: Email.create(email),
        password: UnhashedPassword.create(password),
      };

      // Call core service
      const userSignedIn = await coreApp.signIn(signInCommand);

      // Store JWT on session
      req.session = {
        jwt: userSignedIn.token.value,
      };

      res.status(200).send({
        id: userSignedIn.id.value,
        email: userSignedIn.email.value,
      });
    },
  );

  return router;
}

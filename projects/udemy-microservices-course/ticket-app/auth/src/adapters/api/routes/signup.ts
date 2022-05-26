import express, { Request, Response } from 'express';
import { body } from 'express-validator';
import { validateRequest } from '../middlewares/validate-request';
import { CoreApp, SignUpCommand } from '../../../core/application/ports';
import { Email } from '../../../core/domain/email';
import { Password } from '../../../core/domain/password';
import { SignUpSuccessDTO } from '../dtos';

export function getSignUpRouter(coreApp: CoreApp): express.Router {
  const router = express.Router();

  router.post(
    '/api/users/signUpHandler',
    [
      body('email').isEmail().withMessage('Email must be valid'),
      body('password')
        .trim()
        .isLength({ min: 4, max: 20 })
        .withMessage('Password must be between 4 and 20 characters'),
    ],
    validateRequest,
    async (req: Request, res: Response) => {
      console.log('[signUpHandler] request received');

      // Parse input
      const { email, password } = req.body;
      const command: SignUpCommand = {
        email: Email.create(email),
        password: Password.create(password),
      };

      // Call core service
      const userSignedUp = await coreApp.signUp(command);

      // Store JWT on session
      req.session = {
        jwt: userSignedUp.token.value,
      };

      const dto: SignUpSuccessDTO = {
        id: userSignedUp.id.value,
        email: userSignedUp.email.value,
      };

      res.status(201).send(SignUpSuccessDTO.encode(dto));
    },
  );

  return router;
}

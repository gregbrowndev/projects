import express, { Request, Response } from 'express';
import { body } from 'express-validator';
import { validateRequest } from '../middlewares/validate-request';
import { CoreApp, SignUpCommand } from '../../../core/application/ports';

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
        email,
        password,
      };

      // Call core service
      const userSignedUp = await coreApp.signUp(command);

      // Store JWT on session
      req.session = {
        jwt: userSignedUp.token,
      };

      res.status(201).send({
        id: userSignedUp.id,
        email: userSignedUp.email,
      });
    },
  );

  return router;
}

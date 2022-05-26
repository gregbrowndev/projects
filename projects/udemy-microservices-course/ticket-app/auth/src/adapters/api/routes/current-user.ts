import express, { Request, Response } from 'express';
import { currentUser } from '../middlewares/current-user';
import { CoreApp } from '../../../core/application/ports';
import { CurrentUserDTO } from '../dtos';

export function getCurrentUserRouter(coreApp: CoreApp): express.Router {
  const router = express.Router();

  router.get(
    '/api/users/currentuser',
    currentUser,
    (req: Request, res: Response) => {
      console.log('[currentuser] request received');
      console.log('[currentuser] current user: ', req.currentUser);

      let currentUserDTO: CurrentUserDTO = {
        currentUser: req.currentUser,
      };

      res.send(CurrentUserDTO.encode(currentUserDTO));
    },
  );

  return router;
}

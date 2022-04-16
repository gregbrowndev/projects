import express, { Request, Response } from 'express';
import { CoreApp } from '../../../core/application/ports';

export function getSignOutRouter(coreApp: CoreApp): express.Router {
  const router = express.Router();

  router.post('/api/users/signout', (req: Request, res: Response) => {
    console.log('[signout] request received');
    req.session = null;
    res.send({});
  });

  return router;
}

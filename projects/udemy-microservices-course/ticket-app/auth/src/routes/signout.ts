import express, { Request, Response } from 'express';

const router = express.Router();

router.post('/api/users/signout', (req: Request, res: Response) => {
  console.log('[signout] request received');
  req.session = null;
  res.send({});
});

export { router as signoutRouter };

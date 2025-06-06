import regiserRoutesUnbound from './registerRoutes.unbound';
import registerRoute from './registerRoute/registerRoute';
import { Response } from 'express';

const sendResult = <RB> (res: Response<RB>) => (result: RB): Response => res.send(result);

export default regiserRoutesUnbound(registerRoute, sendResult);

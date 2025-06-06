import jwtAuthenticationUnbound from './jwtAuthentication.unbound';
import { expressJwtSecret } from 'jwks-rsa';
import { expressjwt } from 'express-jwt';

export default jwtAuthenticationUnbound(expressjwt, expressJwtSecret);

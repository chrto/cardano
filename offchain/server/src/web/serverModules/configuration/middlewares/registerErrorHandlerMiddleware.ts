import registerMiddlewares from './registerMiddlewares';
import errorHandler from '../../common/middlewares/errorHandler/errorHandler';

export default registerMiddlewares([errorHandler]);

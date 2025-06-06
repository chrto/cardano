import { isNotInArray } from 'utils/validation';
import hasRequiredFieldsUnbound from './requiredProperties.unbound';

export default hasRequiredFieldsUnbound(isNotInArray);

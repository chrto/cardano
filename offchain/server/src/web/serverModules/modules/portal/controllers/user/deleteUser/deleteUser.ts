import deleteUserUnbound from './deleteUser.unbound';
import isDifferentEntity from 'web/serverModules/common/authorization/validators/differentEntity/differentEntity';

export default deleteUserUnbound(isDifferentEntity);

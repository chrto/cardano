import deleteUserUnbound from './deleteUser.unbound';
import userStorage from 'storage/sequelize/userStorage';

export default deleteUserUnbound(userStorage);

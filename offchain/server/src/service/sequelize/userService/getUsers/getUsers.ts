import getUsersUnbound from './getUsers.unbound';
import userStorage from 'storage/sequelize/userStorage';

export default getUsersUnbound(userStorage);

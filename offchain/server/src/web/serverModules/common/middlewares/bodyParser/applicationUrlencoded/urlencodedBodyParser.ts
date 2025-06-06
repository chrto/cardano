import bodyParserUnbound from './urlencodedBodyParser.unbound';
import * as express from 'express';

export default bodyParserUnbound(express.urlencoded);

const { apiUrl } = require("../config.json")

const getDataFromServer = (url) =>
  fetch(`${apiUrl}/${url}`)
    .then(r => r.json())

export default getDataFromServer

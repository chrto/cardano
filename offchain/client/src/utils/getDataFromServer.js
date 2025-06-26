const { apiUrl } = require("../config.json")

const getDataFromServer = (url) =>
  fetch(`${apiUrl}/${url}`)
    .then(r => r.status < 300
      ? r.json()
      : r.json().then(msg => { throw new Error(JSON.stringify(msg)) })
    )

export default getDataFromServer

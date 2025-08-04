const { apiUrl } = require("../config.json")

const deleteDataFromServer = (url) =>
  fetch(`${apiUrl}/${url}`, {
    method: "delete",
    headers: {
      'Accept': 'application/json'
    }
  })
    .then(r => r.status < 300
      ? r.json()
      : r.json().then(err => { throw err })
    )

export default deleteDataFromServer

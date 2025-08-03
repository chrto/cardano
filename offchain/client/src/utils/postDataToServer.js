const { apiUrl } = require("../config.json")

const postDataToServer = (url, data) =>
  fetch(`${apiUrl}/${url}`, {
    method: "post",
    headers: {
      'Accept': 'application/json',
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(data)
  })
    .then(r => r.status < 300
      ? r.json()
      : r.json().then(err => { throw err })
    )
    .catch(err => {
      throw new Error(`Post data:\ninfo: ${JSON.stringify(err)}`)
    });

export default postDataToServer

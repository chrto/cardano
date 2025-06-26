
const sniff = (desc = '') => (message) => {
  desc !== ''
    ? console.log(`${desc} - ${buildMsg(message)}`)
    : console.log(buildMsg(message));

  return message;
}

const buildMsg = msg =>
  typeof msg === "object"
    ? JSON.stringify(msg)
    : msg

export default sniff


const sniff = (desc = '') => (message) => {
  desc !== ''
    ? console.log(`${desc} - ${message}`)
    : console.log(message);

  return message;
}


export default sniff

const isInt = (value) =>
  !isNaN(value) &&
  parseInt(Number(value)) === value &&
  !isNaN(parseInt(value, 10));

export default isInt;
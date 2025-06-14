const dispatchData = (dispatcher) => (value) => {
  dispatcher(value)
  return value
}

export default dispatchData

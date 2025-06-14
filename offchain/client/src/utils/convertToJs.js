const decodeBytesToUtf8 = (hex) => {
  try {
    return Buffer.from(hex, "hex").toString("utf8");
  } catch {
    return hex; // fallback to raw hex
  }
}

const convertToJs = (data) => {
  if (typeof data === "bigint") {
    return { int: data.toString() };; // BigInt → string
  }

  if (Array.isArray(data)) {
    return data.map(convertToJs);
  }

  if (data && typeof data === "object") {
    if ("constructor" in data && "fields" in data) {
      // Constr { constructor: number, fields: any[] }
      // Plutus Constr
      return {
        constructor: data.index,
        fields: data.fields.map(convertToJs),
      };
    }

    if ("map" in data) {
      // Map from Plutus (Lucid uses `{ map: [...] }`)
      return Object.fromEntries(
        data.map.map(([k, v]) => [convertToJs(k), convertToJs(v)])
      );
    }

    if ("bytes" in data) {
      // Plutus bytes
      return decodeBytesToUtf8(data.bytes);
    }

    if ("list" in data) {
      return data.list.map(convertToJs);
    }

    if ("int" in data) {
      return Number(data.int); // or keep as string: data.int.toString()
    }
  }
  return { bytes: data };
}

export default convertToJs
import React from 'react';
import './Table.css';

function Table({ headers, values }) {
  const getTxId = (idAndIdx) => idAndIdx.split("#")[0];
  const getTxIdx = (idAndIdx) => idAndIdx.split("#")[1];

  return (
    <div className="table-box">
      <div className="table-wrapper">
        <div className="table-scroll">
          <table>
            <thead>
              <tr key={`header`}>
                {headers.map(h => (<th>{h}</th>))}
              </tr>
            </thead>
            <tbody>
              {values.map((row) => (<tr key={row[0]}>{row.map((c, i) => (
                i === 0
                  ? <td><button type="button" onClick={()=> navigator.clipboard.writeText(c)}>Copy</button><a className="table-link" href={"https://preview.cardanoscan.io/transaction/" + getTxId(c)}>{getTxId(c)}</a>#{getTxIdx(c)}</td>
                  : <td>{c}</td>
              ))}</tr>))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

export default Table;

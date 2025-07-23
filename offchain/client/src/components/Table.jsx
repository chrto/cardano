import React from 'react';
import './Table.css';

function Table({ headers, values, selectUtxo }) {
  const EXPL_URL = "https://preview.cardanoscan.io/transaction/"

  const getTxHash = (row) => !!selectUtxo ? row[1] : row[0]
  const getTxIdx = (row) =>  !!selectUtxo ? row[2] : row[1]
  const getTxKey = (row) => `${getTxHash(row)}#${getTxIdx(row)}`

  const renderCheckboxColumn = (columnVal, id, utxoKey) => <td key={id}><input type="checkbox" checked={columnVal} onChange={() => selectUtxo(utxoKey)}></input></td>
  const renderUtxoIdColumn = (columnVal, id) => <td key={id}><a className="table-link" href={`${EXPL_URL}${columnVal}`}>{columnVal}</a></td>
  const renderColumn = (columnVal, id) => <td key={id}>{columnVal}</td>

  return (
    <div className="table-box">
      <div className="table-wrapper">
        <div className="table-scroll">
          <table>
            <thead>
              <tr>
                {headers.map(h => (<th key={h}>{h}</th>))}
              </tr>
            </thead>
            <tbody>
              {values.map((row) => (<tr key={getTxKey(row)}>{row.map((c, i, cs) => (
                i === 0
                  ? !!selectUtxo
                    ? renderCheckboxColumn(c, i, getTxKey(cs))
                    : renderUtxoIdColumn(c, i)
                  : i === 1
                    ? !!selectUtxo
                      ? renderUtxoIdColumn(c, i)
                      : renderColumn(c, i)
                    : renderColumn(c, i)
              ))}</tr>))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

export default Table;

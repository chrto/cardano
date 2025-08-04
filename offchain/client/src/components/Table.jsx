import React from 'react';
import './Table.css';

function Table({ headers, values, selectUtxo, buttons }) {
  const EXPL_URL = "https://preview.cardanoscan.io/transaction/"

  const renderCheckboxColumn = (columnVal, id, utxoKey) => <td key={id}><input type="checkbox" checked={columnVal} onChange={() => selectUtxo(utxoKey)}></input></td>
  const renderUtxoIdColumn = (columnVal, id) => <td key={id}><a className="table-link" href={`${EXPL_URL}${columnVal}`}>{columnVal}</a></td>
  const renderColumn = (columnVal, id) => <td key={id}>{columnVal}</td>
  const renderButtonsCulomn = (buttons, id) => <td style={{ textAlign: 'right' }}>{buttons.map((button, idx) => <button value={ id } key={idx} onClick={button.handler}>{button.title}</button>)}</td>

  const renderRow = (row) => {
    return (
      <tr key={row.key}>
        {row.data.map((c, i, cs) => (
          i === 0
            ? row.select
              ? renderCheckboxColumn(c, i, row.key)
              : row.link
                ? renderUtxoIdColumn(c, i)
                : renderColumn(c, i)
            : i === 1
              ? row.select && row.link
                ? renderUtxoIdColumn(c, i)
                : renderColumn(c, i)
              : renderColumn(c, i)
        ))}

        {!!buttons && (renderButtonsCulomn(buttons, row.key)) || <td></td>}
      </tr>
    );
  };

  return (
    <div className="table-box">
      <div className="table-wrapper">
        <div className="table-scroll">
          <table>
            <thead>
              <tr>
                {headers.map(h => (<th key={h}>{h}</th>))}{<th>{''}</th>}
              </tr>
            </thead>
            <tbody>
              {values.map(renderRow)}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

export default Table;

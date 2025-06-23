import React from 'react';
import './ScriptDetail.css';

function ScriptDetail({ scriptAddress, selectedScript }) {
  return (
    <div className="script-detail">
      <label htmlFor="scriptAddress">Scritp Address</label>
      <textarea id="scriptAddress" value={scriptAddress} readOnly={true} />
      <label htmlFor="scriptType">Scritp Address</label>
      <textarea id="scriptType" value={selectedScript.script.type} readOnly={true} />
      <label htmlFor="scriptCbor">Scritp Address</label>
      <textarea id="scriptCbor" value={selectedScript.script.script} readOnly={true} />
    </div>
  )
}

export default ScriptDetail;

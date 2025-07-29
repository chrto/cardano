import React from "react";
import "./Modal.css";

const Modal = ({ isOpen, isError, onClose, children }) => {
  if (!isOpen) return null;

  return (
    <div className="modal-backdrop" onClick={onClose}>
      <div className={ isError ? "modal-content modal-content-error" : "modal-content"} onClick={(e) => e.stopPropagation()}>
        {children}
        <button className="modal-close" onClick={onClose}>Close</button>
      </div>
    </div>
  );
};

export default Modal;

import { useState, useRef, useEffect } from 'react';
import './AccordionItem.css';

function AccordionItem({ title, children, isOpen, onToggle }) {
  const contentRef = useRef(null);
  const [height, setHeight] = useState(0);

  useEffect(() => {
    if (isOpen && contentRef.current) {
      setHeight(contentRef.current.scrollHeight);
    } else {
      setHeight(0);
    }
  }, [isOpen, title]);

  return (
    <div className="accordion-item">
      <button type="button" className="accordion-header" onClick={onToggle}>
        <h3>{title}</h3>
        <span>{isOpen ? '▼' : '▶'}</span>
      </button>
      <div className="accordion-body-wrapper" style={{ height: `${height}px` }}
      >
        <div className="accordion-body" ref={contentRef}>
          {children}
        </div>
      </div>
    </div>
  );
};

export default AccordionItem;
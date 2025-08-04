import { useState } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import Table from './Table'
import Modal from './Modal';
import deleteDataFromServer from '../utils/deleteDataFromServer';

export default function AccordionScriptsView({ scripts }) {
  const [openIndex, setOpenIndex] = useState(0);
  const [errorMessage, setErrorMessage] = useState(null);
  const [data, setData] = useState(null);
  const [scriptDetail, setScriptDetail] = useState(null);

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  const filterBurn = script => script.category === 'Burn';
  const filterGift = script => script.category === 'Gift';
  const filterFortyTwo = script => script.category === 'FortyTwo';
  const filterVesting = script => script.category === 'Vesting';

  const formToLine = script => ({
    key: script.id,
    link: false,
    select: false,
    data: [
      script.title,
      script.type,
      script.description
    ]
  });

  const handleOnDelete = (e) => {
    e.preventDefault();

    deleteDataFromServer(`cardano/scripts/${e.target.value}`)
      .then(data => setData({
        title: 'Script has been deleted.',
        message: `Script id: ${data.removedScriptId}`
      }))
      .catch(e => setErrorMessage({
        title: 'Script has not been deleted!',
        message: e.message,
        details: e.details
      }))
  }

  const handleOnCbor = (e) => {
    e.preventDefault();

    const script = scripts.find(script => script.id === e.target.value);
    setScriptDetail({script: script.script})
  }

  const handleOnDetail = (e) => {
    e.preventDefault();

    const script = scripts.find(script => script.id === e.target.value);
    setScriptDetail({...script, script: null})
  }

  const handleCloseModal = (e) => {
    e.preventDefault();
    setErrorMessage(null)
    setData(null)
    setScriptDetail(null)
  }

  return (
    <form className="accordion-form">
      <AccordionItem title={"Burn (" + scripts.filter(filterBurn).length + ")"} isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <Table
          headers={['Title', 'Type', 'Description']}
          values={scripts.filter(filterBurn).map(formToLine)}
          buttons={[
            {
              handler: handleOnCbor,
              title: 'CBOR'
            },
            {
              handler: handleOnDetail,
              title: 'Detail'
            },
            {
              handler: handleOnDelete,
              title: 'Delete'
            }
          ]}
        />
      </AccordionItem>

      <AccordionItem title={"Gift (" + scripts.filter(filterGift).length + ")"} isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <Table
          headers={['Title', 'Type', 'Description']}
          values={scripts.filter(filterGift).map(formToLine)}
          buttons={[
            {
              handler: handleOnCbor,
              title: 'CBOR'
            },
            {
              handler: handleOnDetail,
              title: 'Detail'
            },
            {
              handler: handleOnDelete,
              title: 'Delete'
            }
          ]}
        />
      </AccordionItem>

      <AccordionItem title={"FortyTwo (" + scripts.filter(filterFortyTwo).length + ")"} isOpen={openIndex === 2} onToggle={() => toggle(2)}>
        <Table
          headers={['Title', 'Type', 'Description']}
          values={scripts.filter(filterFortyTwo).map(formToLine)}
          buttons={[
            {
              handler: handleOnCbor,
              title: 'CBOR'
            },
            {
              handler: handleOnDetail,
              title: 'Detail'
            },
            {
              handler: handleOnDelete,
              title: 'Delete'
            }
          ]}
        />
      </AccordionItem>

      <AccordionItem title={"Vesting (" + scripts.filter(filterVesting).length + ")"} isOpen={openIndex === 3} onToggle={() => toggle(3)}>
        <Table
          headers={['Title', 'Type', 'Description']}
          values={scripts.filter(filterVesting).map(formToLine)}
          buttons={[
            {
              handler: handleOnCbor,
              title: 'CBOR'
            },
            {
              handler: handleOnDetail,
              title: 'Detail'
            },
            {
              handler: handleOnDelete,
              title: 'Delete'
            }
          ]}
        />
      </AccordionItem>

      <Modal isOpen={!!data || !!errorMessage || !!scriptDetail} isError={!!errorMessage} onClose={handleCloseModal}>
        {
          !!data &&
            <div>
            <h2>{data.title }</h2>
            <p>{data.message }</p>
          </div>
        }
        {
          !!errorMessage &&
            <div>
              <h2>{errorMessage.title}</h2>
              <p>{errorMessage.message}</p>
              <p>{errorMessage.details}</p>
            </div>
        }
        {
          !!scriptDetail && !scriptDetail.script &&
          <div>
            <h2>Script Detail</h2>
            <p>id: {scriptDetail.id}</p>
            <p>type: {scriptDetail.type}</p>
            <p>category: {scriptDetail.category}</p>
            <p>title: {scriptDetail.title}</p>
            <p>description: {scriptDetail.description}</p>
            <p>createdAt: {scriptDetail.createdAt}</p>
            <p>updatedAt: {scriptDetail.updatedAt}</p>
          </div>
        }
        {
          !!scriptDetail && !!scriptDetail.script &&
        <div>
          <h2>Script CBOR</h2>
              <textarea style={{ height: '50dvh', width: '60dvw'}} type='text' name="cbor" value={scriptDetail.script}></textarea>
        </div>
        }
      </Modal>
    </form>
  );
}

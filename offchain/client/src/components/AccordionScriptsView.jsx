import { useState, useImperativeHandle } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import Table from './Table'
import Modal from './Modal';
import deleteDataFromServer from '../utils/deleteDataFromServer';
import getData from '../utils/getDataFromServer';

export default function AccordionScriptsView({ scripts, ref }) {
  const [openIndex, setOpenIndex] = useState(0);
  const [errorMessage, setErrorMessage] = useState(null);
  const [data, setData] = useState(null);
  const [scriptDetail, setScriptDetail] = useState(null);
  const [selectedScript, setSelectedScript] = useState(null);

  // Expose functions to parent
  useImperativeHandle(ref, () => ({
    getSelected() {
      return selectedScript
    },
    deselect() {
      setSelectedScript(null)
    }
  }));

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  const filterBurn = script => script.category === 'Burn';
  const filterGift = script => script.category === 'Gift';
  const filterFortyTwo = script => script.category === 'FortyTwo';
  const filterVesting = script => script.category === 'Vesting';

  const isSelected = (script) => selectedScript === script.id

  const formToLine = script => ({
    key: script.id,
    link: false,
    select: true,
    data: [
      isSelected(script),
      script.title,
      script.type,
      script.description
    ]
  });

  const selectScript = (scriptId) => {
    selectedScript === scriptId
      ? setSelectedScript(null)
      : setSelectedScript(scriptId)
  }

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

  const getScriptAddress = async script =>
    getData(`cardano/script/address?type=${script.type}&script=${script.script}`)
      .then(({ address }) => address)
      .catch(e => {
        console.error(`Can not fetch script address for script ${script.script} from server!\n origin: ${e.message}`)
        return "...";
      })

  const handleOnDetail = (e) => {
    e.preventDefault();

    const script = scripts.find(script => script.id === e.target.value);
    getScriptAddress(script)
      .then(address => setScriptDetail({...script, script: null, address}))
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
          headers={['', 'Title', 'Type', 'Description']}
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
          selectRow={selectScript}
        />
      </AccordionItem>

      <AccordionItem title={"Gift (" + scripts.filter(filterGift).length + ")"} isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <Table
          headers={['', 'Title', 'Type', 'Description']}
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
          selectRow={selectScript}

        />
      </AccordionItem>

      <AccordionItem title={"FortyTwo (" + scripts.filter(filterFortyTwo).length + ")"} isOpen={openIndex === 2} onToggle={() => toggle(2)}>
        <Table
          headers={['', 'Title', 'Type', 'Description']}
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
          selectRow={selectScript}
        />
      </AccordionItem>

      <AccordionItem title={"Vesting (" + scripts.filter(filterVesting).length + ")"} isOpen={openIndex === 3} onToggle={() => toggle(3)}>
        <Table
          headers={['', 'Title', 'Type', 'Description']}
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
          selectRow={selectScript}
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
              {Object.keys(scriptDetail).map(key => !!scriptDetail[key] && <p kye={key}>{ key }: {scriptDetail[key]}</p>)}
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

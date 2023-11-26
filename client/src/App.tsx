import './App.css';
import { useEffect, useState } from 'react'

const baseApiUrl = "http://localhost:3000/api"

export default function App() {
  const [message, setMessage] = useState('')

  const handleChangeMessage = (event: React.ChangeEvent<HTMLTextAreaElement>) => {
    setMessage(event.target.value)
  }

  const initMessage = () => {
    setMessage('')
  }

  const postMessage = async () => {
    await fetch(`${baseApiUrl}/posts`, {
      method: 'POST',
      body: JSON.stringify({ message })
    })
    initMessage()
  }

  useEffect(() => {
    (async () => {
      // await fetch(`${baseApiUrl}`);
    })();
  }, [])

  return (
    <div className="App">
      <div style={postMessageStyle}>
        <textarea style={textareaStyle} value={message} onChange={handleChangeMessage} />
        <button style={buttonStyle} onClick={postMessage}>POST</button>
      </div>
    </div>
  );
}

const postMessageStyle: React.CSSProperties = {
  display: 'grid',
  gridTemplateColumns: 'auto 64px',
  columnGap: '4px'
}

const textareaStyle: React.CSSProperties = {
  resize: "none"
}

const buttonStyle: React.CSSProperties = {
  cursor: 'pointer',
  color: '#ffffff',
  backgroundColor: '#2162e0',
}
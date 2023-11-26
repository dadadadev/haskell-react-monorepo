import './App.css';
import { useEffect, useState } from 'react'

const baseApiUrl = "http://localhost:3000/api"

function App() {
  const [responseText, setResponseText] = useState('');
  const [message, setMessage] = useState('')
  const handleChangeMessage = (event: React.ChangeEvent<HTMLTextAreaElement>) => {
    setMessage(event.target.value)
  }
  const initMessage = () => {
    setMessage('')
  }
  const postMessage = async () => {
    const res = await fetch(`${baseApiUrl}/posts`, {
      method: 'POST',
      body: JSON.stringify({ message })
    })
    const text = await res.text()
    setResponseText(text)
    initMessage()
  }

  useEffect(() => {
    (async () => {
      const result = await fetch(`${baseApiUrl}`);
      const text = await result.text();
      setResponseText(text);
    })();
  }, [])

  return (
    <div className="App">
      <div>{responseText}</div>

      <textarea value={message} onChange={handleChangeMessage} style={{ resize: "none" }} />
      <button onClick={postMessage}>post message</button>

      <button onClick={async () => {
        const res = await fetch(`${baseApiUrl}/foo`)
        if (!res.ok) {
          setResponseText('404')
        }
      }}>invalid request</button>

      <button onClick={() => { window.location.reload() }}>reload</button>
    </div>
  );
}

export default App;

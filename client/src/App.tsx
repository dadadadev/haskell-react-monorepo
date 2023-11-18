import './App.css';
import { useEffect, useState } from 'react'

const baseApiUrl = "http://localhost:3000/api"

function App() {
  const [text, setText] = useState('');

  useEffect(() => {
    (async () => {
      const result = await fetch(`${baseApiUrl}`);
      const text = await result.text();
      setText(text);
    })();
  }, [])

  return (
    <div className="App">
      <div>{text}</div>

      <button onClick={async () => {
        const res = await fetch(`${baseApiUrl}/hello`)
        const text = await res.text()
        setText(text)
      }}>hello</button>

      <button onClick={async () => {
        const res = await fetch(`${baseApiUrl}/foo`)
        if (!res.ok) {
          setText('404')
        }
      }}>invalid request</button>

      <button onClick={() => { window.location.reload() }}>reload</button>
    </div>
  );
}

export default App;

import './App.css';
import { useEffect, useState } from 'react'

const api = () => {
  return fetch("http://localhost:3000/hello");
}

function App() {
  const [text, setText] = useState('');

  useEffect(() => {
    (async () => {
      const result = await api();
      const text = await result.text();
      setText(text);
    })();
  }, [])

  return (
    <div className="App">
      <header className="App-header">
        {text}
      </header>
    </div>
  );
}

export default App;

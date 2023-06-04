import './App.css';
import logo from './logo.svg';
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
        <img src={logo} className="App-logo" alt="logo" />
        {text}
      </header>
    </div>
  );
}

export default App;

import './App.css';
import { useEffect, useState } from 'react'

const baseApiUrl = "http://localhost:3000/api"

export default function App() {
  const [message, setMessage] = useState('')
  const [posts, setPosts] = useState<{ id: number, message: string }[]>([])

  const handleChangeMessage = (event: React.ChangeEvent<HTMLTextAreaElement>) => {
    setMessage(event.target.value)
  }

  const initMessage = () => {
    setMessage('')
  }

  const postMessage = async () => {
    await fetch(`${baseApiUrl}/posts/message`, {
      method: 'POST',
      body: JSON.stringify({ message })
    })

    const req = await fetch(`${baseApiUrl}/posts`);
    const json = await req.json()
    setPosts(json)

    initMessage()
  }

  useEffect(() => {
    (async () => {
      const req = await fetch(`${baseApiUrl}/posts`);
      const json = await req.json()
      setPosts(json)
    })();
  }, [])

  return (
    <div className="App">
      <div style={postMessageStyle}>
        <textarea style={textareaStyle} value={message} onChange={handleChangeMessage} />
        <button style={buttonStyle} onClick={postMessage}>POST</button>
      </div>

      <ul style={listWrapperStyle}>
        {posts.map(post => <li key={post.id} style={listStyle}>{post.message}</li>)}
      </ul>
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

const listWrapperStyle: React.CSSProperties = {
  listStyleType: 'none',
  textAlign: 'left',
}

const listStyle: React.CSSProperties = {
  borderTop: '1px solid #808080',
  padding: '8px 16px'
}
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

  const refetchPosts = async () => {
    const req = await fetch(`${baseApiUrl}/posts`);
    const json = await req.json()
    setPosts(json)
  }

  const postMessage = async () => {
    await fetch(`${baseApiUrl}/posts/message`, {
      method: 'POST',
      body: JSON.stringify({ message })
    })

    refetchPosts()
    initMessage()
  }

  const deleteMessages = async (ids: number[]) => {
    await fetch(`${baseApiUrl}/posts/delete`, {
      method: 'POST',
      body: JSON.stringify({ ids })
    })

    refetchPosts()
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
        <button style={postButtonStyle} disabled={message === ''} onClick={postMessage}>POST</button>
        < button style={deletePostbuttonStyle} disabled={posts.length <= 0} onClick={async () => { deleteMessages(posts.map(({ id }) => id)) }}>DELETE ALL</button>
      </div>

      <ul style={listWrapperStyle}>
        {posts.map(post =>
          <li key={post.id} style={listStyle}>
            {post.message}
            <button style={deletePostbuttonStyle} onClick={async () => { deleteMessages([post.id]) }}>DELETE</button>
          </li>
        )}
      </ul>
    </div>
  );
}

const postMessageStyle: React.CSSProperties = {
  display: 'grid',
  gridTemplateColumns: 'auto 64px 112px',
  columnGap: '4px'
}

const textareaStyle: React.CSSProperties = {
  resize: "none"
}

const postButtonStyle: React.CSSProperties = {
  cursor: 'pointer',
  color: '#ffffff',
  backgroundColor: '#2162e0',
}

const listWrapperStyle: React.CSSProperties = {
  listStyleType: 'none',
  textAlign: 'left',
}

const listStyle: React.CSSProperties = {
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'space-between',
  borderTop: '1px solid #808080',
  minWidth: '320px',
  padding: '8px 16px'
}

const deletePostbuttonStyle: React.CSSProperties = {
  cursor: 'pointer',
  color: '#ffffff',
  backgroundColor: 'red',
}

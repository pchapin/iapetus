import {useState} from 'react';
import reactLogo from './assets/react.svg';
import viteLogo from '/vite.svg';
import './App.css';

function App() {
  const [count, setCount] = useState(0);

  return (
      <>
        <div className="row justify-content-start">
          <div className="col-6">
            <h1>This is a header</h1>
            <p>you clicked {count} times</p>
            <button onClick={() => setCount(count + 1)}>Click me</button>
          </div>
        </div>
      </>
  );
}

export default App;

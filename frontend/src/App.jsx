import {useState} from 'react';
import './App.css';
import IapetusCalendar from './IapetusCalendar.jsx';
import SimpleMDE from 'react-simplemde-editor';
import 'simplemde/src/css/simplemde.css';

function App() {
  const [text, setText] = useState('Hello, World!');

  return (
      <>
        <div className="row justify-content-start">
          <div className="col-9">
            <h1 style={{ textAlign: 'center' }}>Notebook</h1>
            <SimpleMDE
                value={text}
                onChange={setText}
                options={{
                  lineNumbers: false,  // Example option to simplify editor rendering
                  spellChecker: false, // Disable spellcheck if it's adding to the issue
                }}
            />
          </div>
          <div className="col-3">
            <IapetusCalendar />
          </div>
        </div>
      </>
  );
}

export default App;

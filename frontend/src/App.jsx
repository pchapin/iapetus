import React, {useState} from 'react';

import MarkdownIt from 'markdown-it';
import MdEditor from 'react-markdown-editor-lite';
import 'react-markdown-editor-lite/lib/index.css';

import Calendar from 'react-calendar';
import "react-calendar/dist/Calendar.css";

import './App.css';

function App() {
  const [text, setText] = useState('Hello, World!');
  const [date, setDate] = useState(new Date());

  // We need a Markdown parser to render the text.
  const mdParser = new MarkdownIt();

  // When the date changes, update the text to include the new date.
  const handleDateChange = (newDate) => {
    setDate(newDate);
    setText(`# ${newDate.toDateString()}\n\n${text}`);
  };

  return (
      <>
        <div className="row justify-content-start">
          <div className="col-9">
            <h1 style={{textAlign: 'center'}}>Notebook</h1>
            <MdEditor
                value={text}
                style={{height: '500px'}}
                renderHTML={(text) => mdParser.render(text)}
                onChange={({text}) => setText(text)}
            />
          </div>
          <div className="col-3">
            <Calendar
                value={date}
                onChange={handleDateChange}
                minDate={new Date(2020, 0, 1)}
                maxDate={new Date(2039, 11, 31)}
                showNeighboringMonth={false}
            />
            <p>Selected date: {date.toDateString()}</p>
          </div>
        </div>
      </>
  );
}

export default App;

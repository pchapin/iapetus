import React, {useEffect, useState} from 'react';
import {ApolloClient, InMemoryCache, ApolloProvider, useQuery, gql} from '@apollo/client';

import MarkdownIt from 'markdown-it';
import MdEditor from 'react-markdown-editor-lite';
import 'react-markdown-editor-lite/lib/index.css';

import './App.css';
import IapetusCalendar from './IapetusCalendar.jsx';

function App() {
  const [date, setDate] = useState(new Date());
  const [text, setText] = useState('Hello, World!');

  // We need a Markdown parser to render the text.
  const mdParser = new MarkdownIt();

  // When the date changes, update the text to include the new date.
  const handleDateChange = (newDate) => {
    setDate(newDate);
    setText((prevText) => `# ${newDate.toDateString()}\n\n${prevText}`);
  };

  const client = new ApolloClient({
    uri: 'https://main--spacex-l4uc6p.apollographos.net/graphql',
    cache: new InMemoryCache(),
  });

  const QUERY = gql`
    query GetRockets {
      rockets(limit: 10) {
        id
        name
        boosters
        description
      }
    }
  `;

  const Rockets = () => {
    const {loading, error, data} = useQuery(QUERY);

    if (loading) return <p>Loading...</p>;
    if (error) return <p>Error: {error.message}</p>;

    return (
        <ul>
          {data.rockets.map(rocket => (
              <li key={rocket.id}>{rocket.name}</li>
          ))}
        </ul>
    );
  };

  const ImageDisplay = ({ selectedDate }) => {
    const [imageUrl, setImageUrl] = useState(null);
    const [isLoading, setIsLoading] = useState(true)

    const formattedDate = selectedDate.toISOString().split('T')[0];

    useEffect(() => {
      const fetchData = async () => {
        try {
          setIsLoading(true);
          const response = await fetch("https://api.nasa.gov/planetary/apod?api_key=R24wiYiGLZFNVVjOSpn7RLQ82i8ThKabkxpOBNv4&date=" + formattedDate);
          const data = await response.json();
          console.log(data);
          setImageUrl(data.url);
        } catch (error) {
          console.error("Error fetching data:", error);
        } finally {
          setIsLoading(false);
        }
      };

      fetchData();
    }, [formattedDate]);

    return isLoading ? (
        <p>Loading...</p>
    ) : (
        <img src={imageUrl} className="img-fluid" alt="APOD"/>
    );
  };

  return (
      <>
        <div className="row justify-content-start">
          <div className="col-9">
            <h1 style={{textAlign: 'center'}}>Notebook</h1>
            <ApolloProvider client={client}>
              <Rockets/>
            </ApolloProvider>
            <MdEditor
                value={text}
                style={{height: '500px'}}
                renderHTML={(text) => mdParser.render(text)}
                onChange={({text}) => setText(text)}
            />
            <ImageDisplay selectedDate={date}/>
          </div>
          <div className="col-3">
            <IapetusCalendar
                onChange={handleDateChange}
            />
          </div>
        </div>
      </>
  );
}

export default App;

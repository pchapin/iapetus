const express = require('express');
const {MongoClient} = require('mongodb');

const app = express();
const PORT = 3000;
const mongoUri = 'mongodb://localhost:27017'; // Change as needed
const dbName = 'myDatabase';

let db;

// Connect to MongoDB
MongoClient.connect(mongoUri).
    then(client => {
      console.log('Connected to MongoDB');
      db = client.db(dbName);
    }).
    catch(error => console.error('Error connecting to MongoDB:', error));

// Define an endpoint to query the database
app.get('/data', async (req, res) => {
  try {
    const collection = db.collection('myCollection');
    const data = await collection.find({}).toArray(); // Example query: find all documents
    res.json(data);
  } catch (error) {
    console.error('Error querying MongoDB:', error);
    res.status(500).send('Error querying database');
  }
});

app.listen(PORT, () => {
  console.log(`Server running on http://localhost:${PORT}`);
});

// This code sets up an Express server that listens on port 3000. It connects to a local MongoDB
// instance and defines a GET endpoint at /data that queries a collection named myCollection in
// the database myDatabase. The endpoint returns the result of a find query that fetches all
// documents in the collection.
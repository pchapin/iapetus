import express from 'express';
import { MongoClient } from 'mongodb';

//const express = require('express');
//const {MongoClient} = require('mongodb');

const app = express();
const port = 3000;
const mongoUri = 'mongodb://localhost:27017';
const dbName = 'VariableStars';

let db;

// Connect to MongoDB
MongoClient.connect(mongoUri).
    then(client => {
      console.log('Connected to MongoDB');
      db = client.db(dbName);
    }).
    catch(error => console.error('Error connecting to MongoDB:', error));

// Define an endpoint to query the database
app.get('/api/stars', async (req, res) => {
  try {
    const collection = db.collection('stars');
    const data = await collection.find({'constellation': 'Tri'}).toArray();  // An example.
    res.json(data);
  } catch (error) {
    console.error('Error querying MongoDB:', error);
    res.status(500).send('Error querying database');
  }
});

app.listen(PORT, () => {
  console.log(`Server running on http://localhost:${port}`);
});

// This code sets up an Express server to listen on port 3000. It connects to a local MongoDB
// instance and defines a GET endpoint at /api/stars that queries a collection named stars in
// the database VariableStars. The endpoint returns the result of a find query that fetches all
// documents for stars in the constellation of Triangulum.
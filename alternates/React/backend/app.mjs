import express from 'express';

const app = express();
const port = 3000;

// Middleware to parse JSON request bodies.
app.use(express.json());

// Sample in-memory "database" (for demonstration purposes).
let items = [
  { id: 1, name: 'Item 1', quantity: 10 },
  { id: 2, name: 'Item 2', quantity: 5 }
];

// GET: Fetch all items
app.get('/api/items', (req, res) => {
  res.json(items);
});

// GET: Fetch a single item by ID
app.get('/api/items/:id', (req, res) => {
  const foundItem = items.find((item) => item.id === parseInt(req.params.id));
  if (!foundItem) return res.status(404).send('Item not found');
  res.json(foundItem);
});

// POST: Add a new item
app.post('/api/items', (req, res) => {
  const newItem = {
    id: items.length + 1,   // This is unreliable if there were deletions.
    name: req.body.name,
    quantity: req.body.quantity
  };
  items.push(newItem);
  res.status(201).json(newItem);
});

// PUT: Update an existing item
app.put('/api/items/:id', (req, res) => {
  const foundItem = items.find((item) => item.id === parseInt(req.params.id));
  if (!foundItem) return res.status(404).send('Item not found');

  // Use the ES2020 nullish coalescing operator to allow 0 as a valid quantity.
  foundItem.name = req.body.name ?? foundItem.name;
  foundItem.quantity = req.body.quantity ?? foundItem.quantity;
  res.json(foundItem);
});

// DELETE: Remove an item by ID
app.delete('/api/items/:id', (req, res) => {
  const itemIndex = items.findIndex((item) => item.id === parseInt(req.params.id));
  if (itemIndex === -1) return res.status(404).send('Item not found');

  const deletedItem = items.splice(itemIndex, 1);
  res.json(deletedItem);
});

// Start the server
app.listen(port, () => {
  console.log(`API is running at http://localhost:${port}`);
});

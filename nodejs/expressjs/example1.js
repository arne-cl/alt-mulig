const express = require('express')
const app = express()
const port = 3000

app.get('/', (req, res) =>
    res.send('Hello World! Really?'))

app.get('/json', (req, res) => {
    res.json({ hello: "world", foo: 23 })
});

app.listen(port, () => console.log(`Example app listening on port ${port}!`))

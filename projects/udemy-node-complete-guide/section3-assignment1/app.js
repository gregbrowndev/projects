const http = require('http');

const server = http.createServer((req, res) => {
    const url = req.url;
    if (url == '/create-user' && req.method == 'POST') {
        const body = [];
        req.on('data', (chunk) => {
            body.push(chunk);
        });
        return req.on('end', () => {
            const parsedBody = Buffer.concat(body).toString();
            const username = parsedBody.split('=')[1];
            console.log(username);
            res.statusCode = 302;
            res.setHeader('Location', '/');
            return res.end();
        })
    }

    res.setHeader('Content-Type', 'text/html');
    res.write(`<html><head><title>Section 3 Assignment 1</title></head><body>`);

    if (url == '/users') {
        res.write('<ul><li>User 1</li><li>User 2</li><li>User 3</li></ul>')
    } else {
        res.write('<h1>Hello!</h1>')
        res.write(`<form action="/create-user" method="POST">
        <input type="text" name="username" placeholder="Enter your username"/>
        <button type="submit">Submit</button>
        </form>`)
    }
    res.write('</body></html>');
    return res.end()
});

server.listen(3000)
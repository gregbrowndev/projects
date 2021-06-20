const fs = require('fs');

const requestHandler = (req, res) => {
    const url = req.url;
    if (url == '/') {
        res.write('<html>');
        res.write('<body><form action="/message/" method="POST"><input type="text" name="message"></input><button type="submit">Submit</button></form></body>');
        res.write('</html>');
        return res.end();
    }
    else if (url == '/message/' && req.method == 'POST') {
        const body = [];
        req.on('data', (chunk) => {
            body.push(chunk);
            console.log(chunk);
        });
        return req.on('end', () => {
            const parsedBody = Buffer.concat(body).toString();
            console.log(parsedBody);
            const message = parsedBody.split('=')[1];
            fs.writeFile('message.txt', message, (err) => {
                // res.writeHead(302, {'Location': '/'});
                res.statusCode = 302;
                res.setHeader('Location', '/');
                return res.end();
            });
        });
    }
    res.setHeader('Content-Type', 'text/html');
    res.write('<html>');
    res.write('<head><title>My First Page</title></head>');
    res.write('<body><h1>Hello from my Node.js Server!</h1></body>');
    res.write('</html>');
    res.end();
};

module.exports = requestHandler;
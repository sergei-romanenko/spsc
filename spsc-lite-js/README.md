# `SPSC Lite` written in JavaScript

- See
[How to install Node.js](https://nodejs.org/en/learn/getting-started/how-to-install-nodejs).
- Install `Node.js` (version `v20.10.0`) and `nvm`.

To run tests, execute

```bash
npm run test
```

or just

```bash
node test.js
```

Sample tasks, are to be run by means of a local web server. (If you just
open the file `index.html` with a browser, it won't work, for security
reasons.) So, execute

```bash
npm install -g live-server
```

This will install `live-server`, a simple static server that has live
reload built-in. Then run

```bash
live-server
```

in your terminal from the folder `spsc-lite-js`. This will open a new
browser window. Click on the name of a sample task and then click on the
button `Supercompile`.

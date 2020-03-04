Learn Node
==========

These are my notes for Wes Bos' [Learn Node](https://learnnode.com/) online class.


01 - Getting Setup
==================

Install NodeJS
--------------

- install NodeJS 7.6 or above with npm 4 or above

```
# check node version
$ node -v
v10.15.3

$ npm -v
6.10.3
```

- download course materials from https://courses.wesbos.com
- download starter files from https://github.com/wesbos/Learn-Node


Install packages in the starter files directory
-----------------------------------------------

```
$ sudo npm install -g npm
$ npm install
...
added 1179 packages from 817 contributors and audited 10758 packages in 166.636s
found 27 vulnerabilities (10 low, 10 moderate, 7 high)
  run `npm audit fix` to fix them, or `npm audit` for details

$ npm audit fix
+ express-session@1.17.0
+ pug@2.0.4
+ axios@0.19.0
+ body-parser@1.19.0
+ express@4.17.1
+ moment@2.24.0
+ mongoose@5.7.4
added 13 packages from 14 contributors, removed 17 packages and updated 38 packages in 44.992s
fixed 22 of 27 vulnerabilities in 10758 scanned packages
  2 vulnerabilities required manual review and could not be updated
  2 package updates for 3 vulnerabilities involved breaking changes
  (use `npm audit fix --force` to install breaking changes; or refer to `npm audit` for steps to fix these manually)

$ npm audit fix --force
+ css-loader@3.2.0
+ juice@5.2.0
added 83 packages from 69 contributors, removed 75 packages and updated 16 packages in 39.571s
fixed 3 of 5 vulnerabilities in 10800 scanned packages
  2 vulnerabilities required manual review and could not be updated
  2 package updates for 3 vulnerabilities involved breaking changes
  (installed due to `--force` option)

# With NodeJS, you're always doomed!
$ npm audit
┌───────────────┬──────────────────────────────────────────────────────────────┐
│ Low           │ Regular Expression Denial of Service                         │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ Package       │ timespan                                                     │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ Patched in    │ No patch available                                           │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ Dependency of │ forever                                                      │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ Path          │ forever > timespan                                           │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ More info     │ https://npmjs.com/advisories/533                             │
└───────────────┴──────────────────────────────────────────────────────────────┘
┌───────────────┬──────────────────────────────────────────────────────────────┐
│ Low           │ Regular Expression Denial of Service                         │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ Package       │ braces                                                       │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ Patched in    │ >=2.3.1                                                      │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ Dependency of │ forever                                                      │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ Path          │ forever > forever-monitor > chokidar > anymatch > micromatch │
│               │ > braces                                                     │
├───────────────┼──────────────────────────────────────────────────────────────┤
│ More info     │ https://npmjs.com/advisories/786                             │
└───────────────┴──────────────────────────────────────────────────────────────┘
found 2 low severity vulnerabilities in 10315 scanned packages
  2 vulnerabilities require manual review. See the full report for details.
```


Configure IDE
-------------

- we need support for ``.pug`` (HTML template) files
    - [vscodium](https://github.com/VSCodium/vscodium) (free/libre version of Micro$oft VSCode) provides this


02 - Setting up Mongo DB
========================

- we'll use MongoDB 3.2+ as our document store / database
- set up MongoDB locally (or use a hosted service like mlab.com)
    - [how to install on ubuntu with mongodb-maintained packages](https://docs.mongodb.com/manual/tutorial/install-mongodb-on-ubuntu/#install-mongodb-community-edition-using-deb-packages)
    - [how to set a password in MongoDB](https://www.scaleway.com/en/docs/install-and-secure-mongodb-on-ubuntu/)

- default local URL: ``mongodb://127.0.0.1:27017``

- copy ``variables.env.samples`` to ``variables.env``
- in ``variables.env``, replace ``DATABASE`` value with ``mongodb://$USERNAME:$PASSWORD@127.0.0.1:27017/admin``
- add ``variables.env`` to your ``.gitignore`` file

- start MongoDB daemon: ``sudo mongod``
    - 
- test, if mongodb is working

```
$ mongo -u $USERNAME -p --authenticationDatabase admin
> exit
bye
```

- installl [MongoDB Compass](https://www.mongodb.com/download-center/compass) GUI and connect to the database


03 - Starter Files and Environmental Variables
==============================================

- we'll use [ExpressJS](http://expressjs.com/) as our web framework

app.js
------

- imports all the JS/node libraries that we'll use
- loads environment variables from ``variables.env``

start.js
--------

- entrypoint for the app
- connects to our MongoDB database, writes errors to console
- listens for ``GET`` requests on the configured port (default: ``7777``)

Start our example app
---------------------

- ``npm start``
    - looks up ``scripts`` section in ``package.json
    - calls ``node ./start.js`` and lots of other stuff (a file-change watcher, webpack etc)


04 - Core Concept - Routing
===========================

How are URLs routed in ExpressJS?

routes/index.js
--------

- contains a route for ``/``

```
const express = require('express');
const router = express.Router();

router.get('/', (req, res) => {
  res.send('Hey! It works!');
});

module.exports = router;
```

- the routes are then imported into ``app.js`` and used there

```
const routes = require('./routes/index');
[...]
const app = express();
[...]
# all routes starting with '/' will be handled by these routes
app.use('/', routes);
```


05 - Core Concept - Templating.mp4
06 - Core Concept - Template Helpers.mp4
07 - Core Concept - Controllers and the MVC Pattern.mp4
08 - Core Concept - Middleware and Error Handling.mp4
09 - Creating our Store Model.mp4
10 - Saving Stores and using Mixins.mp4
11 - Using Async Await.mp4
12 - Flash Messages.mp4
13 - Querying our Database for Stores.mp4
14 - Creating an Editing Flow for Stores.mp4
15 - Saving Lat and Lng for each store.mp4
16 - Geocoding Data with Google Maps.mp4
17 - Quick Data Visualization Tip.mp4
18 - Uploading and Resizing Images with Middleware.mp4
19 - Routing and Templating Single Stores.mp4
20 - Using Pre-Save hooks to make Unique Slugs.mp4
21 - Custom MongoDB Aggregations.mp4
22 - Multiple Query Promises with Async Await.mp4

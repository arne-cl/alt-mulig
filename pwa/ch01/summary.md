# Chapter 1 - Understanding Progressive Web Apps

PWAs should be:

- responsive
- work without an internet connection
- feel like a native app / installable
- always up-to-date
- easily discoverable / linkable

## 1.2 PWA basics

"These apps aren't packaged and deployed through stores, they're just
websites that took all the right vitamins." (Alex Russell, developer on the
Google Chrome team)

A PWA consists of:

- "normal" website: HTML, CSS and JavaScript
- manifest file (contains metadata)
  - icons, background screen, colors, default orientation; cf. Ch. 6
- Service Workers
  - lets you intercept network requests, e.g. to cache assets for offline use
  - handle push messages


## 1.3 Service Workers: The key to PWAs

"Think of your web requests as planes taking off. Service Worker is the air
traffic controller that routes the requests. It can load from the network or
even off the cache." (Jeff Posnick, Google)

A Service Worker does the following:

- runs in its own global script context (*worker context*)
- isn't tied to a particular web page
- isn't able to modify elements in the web page (no DOM access)
- **works only with HTTPS websites**

A Service Worker is:

- fully async
  - no access to localStorage and synchronous XHR
- event-driven


### 1.3.2 The Service Worker lifecycle

register -> download/parse/execute -> install -> activated

1. call `register()` function
   - during registration, the browser will download, parse and execute the SW  
2. if there's an error, the `register()` promise will reject
3. once the SW executes, the install event is triggered
4. After the installation is completed, the SW is *activated*

**Note: the logic inside the SW will only work after refreshing the page.**

### 1.3.3 A basic Service Worker example

```javascript
<html>
<head>
<title>The best website ever</title>
</head>

<body>
<script>
// Register the service worker
if ('serviceWorker' in navigator) { // check if browser supports SW
    navigator.serviceWorker.register('/sw.js').then(function(registration) {
        // Registration was successful
        console.log('ServiceWorker registration successful with scope: ',
                    registration.scope);
    }).catch(function(err) {
       // registration failed
       console.log('ServiceWorker registration failed: ', err);
    });
}
</script>
</body>
</html>
```

We're using promises (an operation that hasn't completed yet, but is expected
to in the future) instead of callbacks. You can attach `.then()` to the end
and include callbacks inside it for success and failure etc.

If this is your SW:

```javascript
self.addEventListener('fetch', function(event) {
    if (/\.jpg$/.test(event.request.url)) {
        event.respondWith(fetch('/images/unicorn.jpg’));
    }
});
```

it intercept each HTTP request for a `.jpg` file and return a unicorn image
instead.

### 1.3.4 Security considerations

SWs only work via HTTPS. You can:

- run your site locally
- use letsencrypt.org to get a free certificate for your domain
- use Github pages to host your static site for free


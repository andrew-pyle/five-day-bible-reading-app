// INFO
const APP_VERSION = "0.2.0";
const CACHE_NAME = `five-day-bible-reading-app-v${APP_VERSION}`;

const APP_SHELL = [
  // App
  "/",
  "/index.html",
  "/elm.js",
  "/db-interface.js",
  "/data/five-day-reading-plan.json",
  "https://cdn.jsdelivr.net/npm/pouchdb@7.1.1/dist/pouchdb.min.js",
  // Styles & fonts
  "/favicon-512x512.png",
  "/style.css",
  "/fonts/checkbox.svg",
  "/fonts/checkbox-safari-mask.svg",
  // PWA
  "/site.webmanifest",
];

// INSTALL
self.addEventListener("install", (event) => {
  // Tell browser to wait until all the caches have been added
  event.waitUntil(cacheAppShell());
});

/**
 * Creates & Caches the App Shell files while the service worker waits to activate
 * @returns {Promise<void>}
 */
async function cacheAppShell() {
  // Cache app shell in current version cache location while waiting to activate
  const cache = await caches.open(CACHE_NAME);
  await cache.addAll(APP_SHELL);
  //   console.log(`Finished installing. Cache name: ${CACHE_NAME}`);
}

// ACTIVATE
self.addEventListener("activate", (event) => {
  // Tell browser to wait until all the caches have been deleted
  event.waitUntil(clearOtherCaches());
});

/**
 * Deletes all other caches for this origin
 * @returns {Promise<void>}
 */
async function clearOtherCaches() {
  const cacheKeys = await caches.keys();
  // Transform list of old caches into array of promises for deleting them
  //   await Promise.all(
  //     cacheKeys.map(async (key) => {
  for (const key of cacheKeys) {
    if (key !== CACHE_NAME) {
      console.log(`Service Worker is deleting cache: ${key}`);
      await caches.delete(key);
    }
  }
  //}));
}

// FETCH
self.addEventListener("fetch", (event) => {
  event.respondWith(networkFirstCacheFallback(event.request));
});

/**
 * Serves the content from the network if available, and from the cache if not
 * See: https://glebbahmutov.com/blog/async-functions-in-sw/
 * @param {Request} request The request to intercept & serve a response for
 * @returns {Response} The response for the intercepted request
 */
async function networkFirstCacheFallback(request) {
  try {
    // Try to get a fresh response from the network
    const networkResponse = await fetch(request, {
      mode: "cors",
      credentials: "same-origin",
    });
    // console.log(networkResponse); // Debug

    // Cache a 'ok' network response
    if (networkResponse && networkResponse.ok === true) {
      // Cache the fresh network response
      const responseClone = networkResponse.clone();
      const cache = await caches.open(CACHE_NAME);
      await cache.put(request, responseClone);
    }

    // Send the network response to the browser, good or bad
    return networkResponse;
  } catch (err) {
    // Try the cache. Maybe we are just offline?
    const cacheResponse = await caches.match(request);
    // Cache hit
    if (cacheResponse) {
      return cacheResponse;
    }
    // Cache Miss
    else {
      // If all else fails, crash the site
      Promise.reject("no response available");
    }
  }
}

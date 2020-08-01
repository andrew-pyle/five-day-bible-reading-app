// INFO
const APP_VERSION = "0.2.1";
const CACHE_NAME = `five-day-bible-reading-app-v${APP_VERSION}`;

const APP_SHELL = [
  // App
  "/",
  "/index.html",
  "/elm.js",
  "/db-interface.js",
  "/data/five-day-reading-plan.json",
  // JS Libraries
  "https://cdn.jsdelivr.net/npm/pouchdb@7.1.1/dist/pouchdb.min.js",
  // Styles
  "/style.css",
  "/fonts/checkbox.svg",
  "/fonts/checkbox-safari-mask.svg",
  // Google Fonts
  "https://fonts.googleapis.com/css2?family=Lexend+Deca&display=swap",
  // PWA
  "/favicon-512x512.png",
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
  event.respondWith(cacheFirstNetworkFallback(event.request));
});

/**
 * Retrieves resource from cache, falling back to a network request upon a
 * cache miss.
 * See: https://developers.google.com/web/fundamentals/instant-and-offline/offline-cookbook#cache-falling-back-to-network
 * @param {Request} request The request to intercept & serve a response for
 * @returns {Promise<Response>} The response for the intercepted request
 */
async function cacheFirstNetworkFallback(request) {
  const cacheResponse = await caches.match(request);
  return cacheResponse || fetch(request);
}

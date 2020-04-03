// PouchDB is globally available from classic <script> in index.html

export const db = new PouchDB({
  name: "Five_Day_Bible_Reading_Plan_App_User_Reading_Progress",
  adapter: "idb",
});

/** @typedef {{_id: string, _rev?: string, userData: Object}} Doc */

/**
 * Get the stored local data for the Elm app. Return `undefined` if there isn't
 * any app data stored
 * @param {string} appId ID of the Elm application. Just one for the app.
 * @returns {void}
 */
export async function getLocalUserProgress(appId) {
  try {
    const result = await db.get(appId);
    return result.userData;
  } catch (err) {
    // No return value. Just keep on working & log Error
    console.error(err);
  }
}

/**
 * Updates the user progress data from Elm if it exists or
 *  creates user progress if nothing is saved already
 * @param {string} appId Elm app's storage key
 * @param {Object} data PouchDB document containing the user's data to store
 * @returns {void}
 */
export async function serializeUserProgress(appId, data) {
  try {
    // Fetch doc from PouchDB. We need the whole doc including the _rev
    // to update without potential conflicts
    const oldDoc = await db.get(appId);

    // If user data already exists, update it.
    const updatedDoc = {
      ...oldDoc,
      userData: data,
    };
    db.put(updatedDoc);
    // No return value
  } catch (err) {
    // No old data was found in the browser
    if (err.status === 404) {
      // Create user storage if none exists
      db.put({
        _id: appId,
        userData: data,
      });
      // No return value
    } else {
      // Handle Unexpected errors here
      console.error(err);
    }
  }
}

import {PLACE_ADDED, REMOVE_PLACE, SET_PLACES, START_ADD_PLACE} from './actionTypes';
import {uiStartLoading, uiStopLoading} from './index';
import {authGetToken} from './auth';

export const addPlace = (placeName, location, image) => {
  return dispatch => {
    let authToken;
    dispatch(uiStartLoading());
    dispatch(authGetToken())
      .catch(() => {
        alert("No valid token found!");
      })
      .then(token => {
        authToken = token;
        return fetch(`https://us-central1-awesome-places-1515966501374.cloudfunctions.net/storeImage`, {
          method: "POST",
          body: JSON.stringify({
            image: image.base64
          }),
          headers: {
            "Authorization": `Bearer ${token}`
          }
        });
      })
      .then(res => {
        if (res.ok) {
          return res.json()
        } else {
          throw(new Error());
        }
      })
      .then(parsedRes => {
        const placeData = {
          name: placeName,
          location: location,
          image: parsedRes.imageUrl
        };
        return fetch(`https://awesome-places-1515966501374.firebaseio.com/places.json?auth=${authToken}`, {
          method: "POST",
          body: JSON.stringify(placeData)
        })
      })
      .then(res => {
        if (res.ok) {
          return res.json()
        } else {
          throw(new Error());
        }
      })
      .then(parsedRes => {
        console.log(parsedRes);
        dispatch(uiStopLoading());
        dispatch(placeAdded());
      })
      .catch(err => {
        console.log(err);
        alert("Something went wrong, please try again!");
        dispatch(uiStopLoading());
      });
  };
};

export const placeAdded = () => {
  return {
    type: PLACE_ADDED
  }
};

export const startAddPlace = () => {
  return {
    type: START_ADD_PLACE
  }
};

export const getPlaces = () => {
  return dispatch => {
    console.log("getPlaces dispatched");
    dispatch(authGetToken())
      .catch(() => {
        alert("No valid token found!");
      })
      .then(token => {
        return fetch(`https://awesome-places-1515966501374.firebaseio.com/places.json?auth=${token}`);
      })
      .then(res => {
        if (res.ok) {
          return res.json()
        } else {
          throw(new Error());
        }
      })
      .then(parsedRes => {
        const places = [];
        for (const key in parsedRes) {
          if (parsedRes.hasOwnProperty(key)) {
            places.push({
              ...parsedRes[key],
              key: key,
              image: {
                uri: parsedRes[key].image
              }
            })
          }
        }
        dispatch(setPlaces(places))
      })
      .catch(err => {
        console.log(err);
        alert("Something went wrong, please try again!");
      });
  };
};

export const setPlaces = places => {
  return {
    type: SET_PLACES,
    places: places
  }
};

export const deletePlace = (key) => {
  return dispatch => {
    dispatch(authGetToken())
      .catch(() => {
        alert("No valid token found!");
      })
      .then(token => {
        return fetch(`https://awesome-places-1515966501374.firebaseio.com/places/${key}/.json?auth=${token}`, {
          method: "DELETE"
        });
      })
      .then(res => {
        if (res.ok) {
          return res.json()
        } else {
          throw(new Error());
        }
      })
      .then(parsedRes => {
        console.log("deleted response", parsedRes);
        dispatch(removePlace(key));
      })
      .catch(err => {
        console.log(err);
        alert("Something went wrong, please try again!");
      });
  };
};

export const removePlace = key => {
  return {
    type: REMOVE_PLACE,
    key: key
  }
};
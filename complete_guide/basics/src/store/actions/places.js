import {REMOVE_PLACE, SET_PLACES} from './actionTypes';
import {uiStartLoading, uiStopLoading} from './index';

export const addPlace = (placeName, location, image) => {
  return dispatch => {
    dispatch(uiStartLoading());
    fetch("https://us-central1-awesome-places-1515966501374.cloudfunctions.net/storeImage", {
      method: "POST",
      body: JSON.stringify({
        image: image.base64
      })
    })
      .catch(err => {
        console.log(err);
        alert("Something went wrong, please try again!");
        dispatch(uiStopLoading());
      })
      .then(res => res.json())
      .then(parsedRes => {
        const placeData = {
          name: placeName,
          location: location,
          image: parsedRes.imageUrl
        };
        return fetch("https://awesome-places-1515966501374.firebaseio.com/places.json", {
          method: "POST",
          body: JSON.stringify(placeData)
        })
      })
      .catch(err => {
        console.log(err);
        alert("Something went wrong, please try again!");
        dispatch(uiStopLoading());
      })
      .then(res => res.json())
      .then(parsedRes => {
        console.log(parsedRes);
        dispatch(uiStopLoading());
      });
  };
};

export const getPlaces = () => {
  return dispatch => {
    fetch("https://awesome-places-1515966501374.firebaseio.com/places.json")
      .catch(err => {
        console.log(err);
        alert("Something went wrong, please try again!");
      })
      .then(res => res.json())
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
    fetch("https://awesome-places-1515966501374.firebaseio.com/places/" + key + "/.json", {
      method: "DELETE"
    })
      .catch(err => {
        console.log(err);
        alert("Something went wrong, please try again!");
      })
      .then(res => res.json())
      .then(parsedRes => {
        console.log("deleted response", parsedRes);
        dispatch(removePlace(key));
      });
  };
};

export const removePlace = key => {
  return {
    type: REMOVE_PLACE,
    key: key
  }
};
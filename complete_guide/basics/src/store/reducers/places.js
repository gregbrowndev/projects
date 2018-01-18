import {ADD_PLACE, DELETE_PLACE} from '../actions/actionTypes';

const initialState = {
  places: [],
};

const reducer = (state = initialState, action) => {
  switch (action.type) {
    case ADD_PLACE:
      return {
        ...state,
        places: state.places.concat({
          key: Math.random(),
          name: action.placeName,
          image: {
            uri: 'http://vacationidea.com/pix/img25Hy8R/articles/most-beautiful-places-in-the-world_g25_mobi.jpg'
          },
          location: action.location
        })
      };
    case DELETE_PLACE:
      return {
        ...state,
        places: state.places.filter(place => {
          return place.key !== action.key;
        }),
        selectedPlace: null
      };
    default:
      return state;
  }
};

export default reducer;
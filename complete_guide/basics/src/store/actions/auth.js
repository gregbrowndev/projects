import {AsyncStorage} from 'react-native';
import {AUTH_SET_TOKEN, TRY_AUTH} from './actionTypes';
import {uiStartLoading, uiStopLoading} from './ui';
import startMainTabs from '../../screens/MainTabs/startMainTabs';

export const tryAuth = (authData, authMode) => {
  return dispatch => {
    dispatch(uiStartLoading());
    const apiKey = "AIzaSyBm4kDEppFyS15xmEWsXbXLTTgKeE8-65w";
    let url = `https://www.googleapis.com/identitytoolkit/v3/relyingparty/verifyPassword?key=${apiKey}`;
    if (authMode === 'signup') {
      url = `https://www.googleapis.com/identitytoolkit/v3/relyingparty/signupNewUser?key=${apiKey}`
    }

    fetch(url, {
      method: "POST",
      body: JSON.stringify({
        email: authData.email,
        password: authData.password,
        returnSecureToken: true
      }),
      headers: {
        "Content-Type": "application/json"
      }
    })
      .then(res => res.json())
      .then(parsedRes => {
        dispatch(uiStopLoading());
        if (!parsedRes.idToken) {
          alert("Authentication failed, please try again");
        } else {
          dispatch(authStoreToken(parsedRes.idToken, parsedRes.expiresIn));
          startMainTabs();
        }
      })
      .catch(err => {
        console.log(err);
        alert("Authentication failed, please try again");
        dispatch(uiStopLoading());
      });
  };
};

export const authStoreToken = (token, expiresIn) => {
  return dispatch => {
    const now = new Date();
    const expiryDate = now.getTime() + expiresIn * 1000;
    dispatch(authSetToken(token));
    AsyncStorage.setItem("ap:auth:token", token);
    AsyncStorage.setItem("ap:auth:expiryDate", expiryDate.toString());
  }
};

export const authSetToken = token => {
  return {
    type: AUTH_SET_TOKEN,
    token: token
  }
};

export const authGetToken = () => {
  return (dispatch, getState) => {
    return new Promise((resolve, reject) => {
      const token = getState().auth.token;
      if (!token) {
        let fetchedToken;
        AsyncStorage.getItem("ap:auth:token")
          .catch(error => reject())
          .then(tokenFromStorage => {
            if (!tokenFromStorage) {
              reject();
            }
            fetchedToken = tokenFromStorage;
            return AsyncStorage.getItem("ap:auth:expiryDate");
          })
          .then(expiryDate => {
            if (new Date(parseInt(expiryDate)) > new Date()) {
              dispatch(authSetToken(fetchedToken));
              resolve(fetchedToken)
            } else {
              reject();
            }
          })
          .catch(err => reject());
      } else {
        resolve(token);
      }
    })
      .catch(err => {
        dispatch(authClearStorage());
      });
  }
};

export const authAutoSignIn = () => {
  return dispatch => {
    dispatch(authGetToken())
      .then(token => {
        startMainTabs();
      })
      .catch(err => console.log("Failed to fetch token"));
  }
};

export const authClearStorage = () => {
  return dispatch => {
    AsyncStorage.removeItem("ap:auth:token");
    AsyncStorage.removeItem("ap:auth:expiryDate");
  };
};
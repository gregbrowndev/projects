import {AsyncStorage} from 'react-native';
import {AUTH_SET_TOKEN, TRY_AUTH} from './actionTypes';
import {uiStartLoading, uiStopLoading} from './ui';
import startMainTabs from '../../screens/MainTabs/startMainTabs';

const API_KEY = "AIzaSyBm4kDEppFyS15xmEWsXbXLTTgKeE8-65w";

export const tryAuth = (authData, authMode) => {
  return dispatch => {
    dispatch(uiStartLoading());
    let url = `https://www.googleapis.com/identitytoolkit/v3/relyingparty/verifyPassword?key=${API_KEY}`;
    if (authMode === 'signup') {
      url = `https://www.googleapis.com/identitytoolkit/v3/relyingparty/signupNewUser?key=${API_KEY}`
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
          dispatch(authStoreToken(
            parsedRes.idToken,
            parsedRes.expiresIn,
            parsedRes.refreshToken
          ));
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

export const authStoreToken = (token, expiresIn, refreshToken) => {
  return dispatch => {
    const now = new Date();
    const expiryDate = now.getTime() + 20 * 1000;
    dispatch(authSetToken(token));
    AsyncStorage.setItem("ap:auth:token", token);
    AsyncStorage.setItem("ap:auth:expiryDate", expiryDate.toString());
    AsyncStorage.setItem("ap:auth:refreshToken", refreshToken);
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
    console.log("authGetToken: dispatched");
    return new Promise((resolve, reject) => {
      const token = getState().auth.token;
      if (!token) {
        console.log("authGetToken: no token in store");
        let fetchedToken;
        AsyncStorage.getItem("ap:auth:token")
          .catch(error => {
            console.log("authGetToken: error trying to get token from storage", error);
            reject();
            return;
          })
          .then(tokenFromStorage => {
            if (!tokenFromStorage) {
              console.log("authGetToken: no token returned from storage");
              reject();
              return;
            }
            console.log("authGetToken: received token from storage");
            fetchedToken = tokenFromStorage;
            return AsyncStorage.getItem("ap:auth:expiryDate");
          })
          .then(expiryDate => {
            console.log('authGetToken: Checking token expiry date');
            if (new Date(parseInt(expiryDate)) > new Date()) {
              console.log("Token in date");
              dispatch(authSetToken(fetchedToken));
              resolve(fetchedToken)
            } else {
              console.log("Token expired");
              reject();
              return;
            }
          })
          .catch(err => {
            console.log("authGetToken: error trying to get token from storage (final catch)", err);
            reject();
            return;
          });
      } else {
        console.log("authGetToken: got token from store");
        resolve(token);
      }
    })
      .catch(err => {
        // Fallback if no token or token expired, try to use refreshToken to get a new token
        console.log("authGetToken: couldn't get token, will fallback to refreshToken");
        dispatch(authRefreshToken())
          .then(token => {
            console.log("authGetToken got refreshed token!", token);
            resolve(token);
          })
          .catch(err => {
            console.log("authGetToken could not get refreshed token", token);
            dispatch(authClearStorage());
            reject();
          });
      });
  }
};

export const authAutoSignIn = () => {
  return dispatch => {
    console.log("authAutoSignIn: dispatched");
    dispatch(authGetToken())
      .then(token => {
        startMainTabs();
      })
      .catch(err => console.log("Failed to get token"));
  }
};

export const authClearStorage = () => {
  return dispatch => {
    console.log("authClearStorage: dispatched");
    AsyncStorage.removeItem("ap:auth:token");
    AsyncStorage.removeItem("ap:auth:refreshToken");
    AsyncStorage.removeItem("ap:auth:expiryDate");
  };
};

export const authRefreshToken = () => {
  return dispatch => {
    console.log("authRefreshToken: dispatched");
    return AsyncStorage.getItem("ap:auth:refreshToken")
      .then(refreshToken => {
        return fetch(`https://securetoken.googleapis.com/v1/token?key=${API_KEY}`, {
          method: "POST",
          headers: {
            "Content-Type": "application/x-www-form-urlencoded"
          },
          body: `grant_type=refresh_token&refresh_token=${refreshToken}`
        })
      })
      .then(res => res.json())
      .then(parsedRes => {
        const newToken = parsedRes.id_token;
        if (newToken) {
          console.log("authRefreshToken: received refreshed token");
          dispatch(authStoreToken(
            newToken,
            parsedRes.expires_in,
            parsedRes.refresh_token
          ));
          return newToken;
        } else {
          console.log("authRefreshToken: no refreshed token received");
          reject();
        }
      })
      .catch(err => console.log('authRefreshToken: failed to get refreshToken from storage'))
  };
};

export const removeToken = () => {
  return dispatch => {
    dispatch(authSetToken(null));
  }
};

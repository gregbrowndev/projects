import React, {Component} from 'react';
import {StyleSheet, View, Button} from 'react-native';
import {connect} from 'react-redux';

import startMainTabs from '../MainTabs/startMainTabs';
import {tryAuth} from '../../store/actions';
import Signup from '../../components/Auth/Signup';
import Login from '../../components/Auth/Login';

class AuthScreen extends Component {
  state = {
    authMode: 'login',
  };

  switchAuthModeHandler = () => {
    this.setState(prevState => {
      return {
        authMode: prevState.authMode === 'login' ? 'signup' : 'login'
      };
    })
  };

  loginHandler = authData => {
    console.log('login pressed', authData);
    this.props.onLogin(authData);
  };

  signupHandler = authData => {
    console.log('signup pressed', authData);
  };

  render() {
    const login = (
      <Login
        isLoading={this.props.isLoading}
        onSubmit={this.loginHandler}
        onSwitchAuthMode={this.switchAuthModeHandler}
      />
    );

    const signup = (
      <Signup
        isLoading={this.props.isLoading}
        onSubmit={this.signupHandler}
        onSwitchAuthMode={this.switchAuthModeHandler}
      />
    );

    return this.state.authMode === 'login' ? login : signup
  }
}

const mapStateToProps = state => {
  return {
    isLoading: state.ui.isLoading
  }
};

const mapDispatchToProps = dispatch => {
  return {
    onLogin: authData => dispatch(tryAuth(authData))
  };
};

export default connect(mapStateToProps, mapDispatchToProps)(AuthScreen);
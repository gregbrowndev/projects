import React, {Component} from 'react';
import {StyleSheet, View, Button} from 'react-native';
import {connect} from 'react-redux';

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

  authHandler = authData => {
    this.props.onTryAuth(authData, this.state.authMode);
  };

  render() {
    const login = (
      <Login
        isLoading={this.props.isLoading}
        onSubmit={this.authHandler}
        onSwitchAuthMode={this.switchAuthModeHandler}
      />
    );

    const signup = (
      <Signup
        isLoading={this.props.isLoading}
        onSubmit={this.authHandler}
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
    onTryAuth: (authData, authMode) => dispatch(tryAuth(authData, authMode))
  };
};

export default connect(mapStateToProps, mapDispatchToProps)(AuthScreen);
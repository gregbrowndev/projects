import React, {Component} from 'react';
import {View, Text, Button, TextInput, StyleSheet, ImageBackground, Dimensions} from 'react-native';

import startMainTabs from '../MainTabs/startMainTabs';
import DefaultInput from '../../components/UI/DefaultInput/DefaultInput';
import HeadingText from '../../components/UI/HeadingText/HeadingText';
import MainText from '../../components/UI/MainText/MainText';
import backgroundImage from '../../assets/background.jpg';
import ButtonWithBackground from '../../components/UI/ButtonWithBackground/ButtonWithBackground';
import validate from '../../utility/validation';

class AuthScreen extends Component {
  state = {
    viewMode: Dimensions.get('window').height > 500 ? "portrait" : "landscape",
    controls: {
      email: {
        value: "",
        valid: false,
        validationRules: {
          isEmail: true
        }
      },
      password: {
        value: "",
        valid: false,
        validationRules: {
          minLength: 6
        }
      },
      confirmPassword: {
        value: "",
        valid: false,
        validationRules: {
          equalTo: 'password'
        }
      }
    }
  };

  constructor(props) {
    super(props);
    Dimensions.addEventListener("change", this.updateStyles);
  }

  componentWillUnmount() {
    Dimensions.removeEventListener("change", this.updateStyles)
  }

  updateStyles = dims => {
    this.setState({
      viewMode: dims.window.height > 500 ? "portrait" : "landscape"
    });
  };

  loginHandler = () => {
    startMainTabs();
  };

  updateInputState = (key, val) => {
    const controls = this.state.controls;
    const control = controls[key];

    let connectedValue = {};
    if (control.validationRules.equalTo != null) {
      const equalTo = control.validationRules.equalTo;
      const equalVal = controls[equalTo].value;

      connectedValue = {
        ...connectedValue,
        equalTo: equalVal
      }
    }

    // Need to make sure we can re-validate confirmPassword if updating password, so pass connectedValue
    if (key === "password") {
      connectedValue = {
        ...connectedValue,
        equalTo: val
      }
    }

    this.setState(prevState => {
      const prevControls = prevState.controls;

      // Get state of previous controls and always do check that password and confirmPassword match
      const prevConfirmPassword = prevControls.confirmPassword;
      let controls = {
        ...prevControls,
        confirmPassword: {
          ...prevConfirmPassword,
          valid: key === 'password'
            ? validate(prevConfirmPassword.value, prevConfirmPassword.validationRules, connectedValue)
            : prevConfirmPassword.valid
        }
      };

      // Now update state of control being changed
      const prevControl = prevControls[key];
      controls[key] = {
        ...prevControl,
        value: val,
        valid: validate(val, prevControl.validationRules, connectedValue)
      };

      return {controls: controls};
    });
  };

  render() {
    let headingText = null;

    if (this.state.viewMode === "portrait") {
      headingText = (
        <MainText>
          <HeadingText>Please Login</HeadingText>
        </MainText>
      );
    }

    return (
      <ImageBackground source={backgroundImage} style={styles.backgroundImage}>
        <View style={styles.container}>
          {headingText}
          <ButtonWithBackground backgroundColor="#29aaf4">Switch to Login</ButtonWithBackground>
          <View style={styles.inputContainer}>
            <DefaultInput
              placeholder="Your Email Address"
              style={styles.input}
              value={this.state.controls.email.value}
              onChangeText={(val) => this.updateInputState('email', val)}
            />
            <View style={
              this.state.viewMode === "portrait"
                ? styles.portraitPasswordContainer
                : styles.landscapePasswordContainer
            }>
              <View style={
                this.state.viewMode === "portrait"
                  ? styles.portraitPasswordWrapper
                  : styles.landscapePasswordWrapper
              }>
                <DefaultInput
                  placeholder="Password"
                  style={styles.input}
                  value={this.state.controls.password.value}
                  onChangeText={(val) => this.updateInputState('password', val)}
                />
              </View>
              <View style={
                this.state.viewMode === "portrait"
                  ? styles.portraitPasswordWrapper
                  : styles.landscapePasswordWrapper
              }>
                <DefaultInput
                  placeholder="Confirm Password"
                  style={styles.input}
                  value={this.state.controls.confirmPassword.value}
                  onChangeText={(val) => this.updateInputState('confirmPassword', val)}
                />
              </View>
            </View>
          </View>
          <ButtonWithBackground backgroundColor="#29aaf4" onPress={this.loginHandler}>Submit</ButtonWithBackground>
        </View>
      </ImageBackground>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'center',
    justifyContent: 'center'
  },
  inputContainer: {
    width: "80%"
  },
  input: {
    backgroundColor: "#eee",
    borderColor: "#bbb"
  },
  backgroundImage: {
    width: "100%",
    flex: 1
  },
  portraitPasswordContainer: {
    flexDirection: "column",
    justifyContent: "flex-start"
  },
  landscapePasswordContainer: {
    flexDirection: "row",
    justifyContent: "space-between"
  },
  portraitPasswordWrapper: {
    width: "100%"
  },
  landscapePasswordWrapper: {
    width: "45%"
  }
});

export default AuthScreen;
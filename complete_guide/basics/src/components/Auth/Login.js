import React, {Component} from 'react';
import {
  View,
  Text,
  Button,
  TextInput,
  StyleSheet,
  ImageBackground,
  Dimensions,
  KeyboardAvoidingView,
  Keyboard,
  TouchableWithoutFeedback
} from 'react-native';

import validate from '../../utility/validation';
import ButtonWithBackground from '../UI/ButtonWithBackground/ButtonWithBackground';
import DefaultInput from '../UI/DefaultInput/DefaultInput';
import HeadingText from '../../components/UI/HeadingText/HeadingText';
import MainText from '../UI/MainText/MainText';
import backgroundImage from '../../assets/background.jpg';


class Login extends Component {
  state = {
    viewMode: Dimensions.get('window').height > 500 ? "portrait" : "landscape",
    controls: {
      email: {
        value: "",
        valid: false,
        validationRules: {
          isEmail: true
        },
        touched: false
      },
      password: {
        value: "",
        valid: false,
        validationRules: {
          minLength: 6
        },
        touched: false
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

    this.setState(prevState => {
      const prevControls = prevState.controls;

      // Get state of previous controls
      let controls = {
        ...prevControls
      };

      // Now update state of control being changed
      const prevControl = prevControls[key];
      controls[key] = {
        ...prevControl,
        value: val,
        valid: validate(val, prevControl.validationRules, connectedValue),
        touched: true
      };

      return {controls: controls};
    });
  };

  submitHandler = () => {
    const authData = {
      email: this.state.controls.email.value,
      password: this.state.controls.password.value
    };
    this.props.onSubmit(authData);
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
      <TouchableWithoutFeedback onPress={Keyboard.dismiss}>
        <View style={{flex: 1}}>
          <ImageBackground source={backgroundImage} style={styles.backgroundImage}>
            <KeyboardAvoidingView behavior="padding" style={styles.container}>
              {headingText}
              <ButtonWithBackground
                onPress={this.props.onSwitchAuthMode}
                backgroundColor="#29aaf4">
                Switch to Signup
              </ButtonWithBackground>
              <View style={styles.inputContainer}>
                <DefaultInput
                  placeholder="Your Email Address"
                  style={styles.input}
                  value={this.state.controls.email.value}
                  onChangeText={(val) => this.updateInputState('email', val)}
                  valid={this.state.controls.email.valid}
                  touched={this.state.controls.email.touched}
                  autoCapitalize="none"
                  autoCorrect={false}
                  keyboardType="email-address"
                />
                <DefaultInput
                  placeholder="Password"
                  style={styles.input}
                  value={this.state.controls.password.value}
                  onChangeText={(val) => this.updateInputState('password', val)}
                  valid={this.state.controls.password.valid}
                  touched={this.state.controls.password.touched}
                  secureTextEntry
                />
              </View>
              <ButtonWithBackground
                backgroundColor="#29aaf4"
                onPress={this.submitHandler}
                // disabled={
                //   !(this.state.controls.email.valid
                //     && this.state.controls.password.valid)
                // }
              >
                Submit
              </ButtonWithBackground>
            </KeyboardAvoidingView>
          </ImageBackground>
        </View>
      </TouchableWithoutFeedback>
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
  }
});

export default Login;
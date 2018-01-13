import React, {Component} from 'react';
import {View, Text, Button, TextInput, StyleSheet, ImageBackground, Dimensions} from 'react-native';

import startMainTabs from '../MainTabs/startMainTabs';
import DefaultInput from '../../components/UI/DefaultInput/DefaultInput';
import HeadingText from '../../components/UI/HeadingText/HeadingText';
import MainText from '../../components/UI/MainText/MainText';
import backgroundImage from '../../assets/background.jpg';
import ButtonWithBackground from '../../components/UI/ButtonWithBackground/ButtonWithBackground';

class AuthScreen extends Component {
  state = {
    viewMode: Dimensions.get('window').height > 500 ? "portrait" : "landscape"
  };

  constructor(props) {
    super(props);
    Dimensions.addEventListener("change", (dims) => {
      this.setState({
        viewMode: Dimensions.get('window').height > 500 ? "portrait" : "landscape"
      });
    });
  }

  loginHandler = () => {
    startMainTabs();
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
            <DefaultInput placeholder="Your Email Address" style={styles.input}/>
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
                <DefaultInput placeholder="Password" style={styles.input}/>
              </View>
              <View style={
                this.state.viewMode === "portrait"
                  ? styles.portraitPasswordWrapper
                  : styles.landscapePasswordWrapper
              }>
                <DefaultInput placeholder="Confirm Password" style={styles.input}/>
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
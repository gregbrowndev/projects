import React from 'react';
import {
  Platform,
  TouchableOpacity,
  TouchableNativeFeedback,
  TouchableHighlight,
  Text,
  View,
  StyleSheet
} from 'react-native';

const buttonWithBackground = props => {
  const content = (
    <View style={[
      styles.button,
      {backgroundColor: props.backgroundColor},
      props.disabled ? styles.disabled : null
    ]}>
      <Text style={props.disabled ? styles.disabledText : null}>
        {props.children}
      </Text>
    </View>
  );

  if (props.disabled) {
    return content;
  }

  type MyButton = TouchableOpacity;
  if (Platform.OS === 'android') {
    MyButton = TouchableNativeFeedback;
  }

  return (
    <MyButton onPress={props.onPress}>
      {content}
    </MyButton>
  )
    ;
};

const styles = StyleSheet.create({
  button: {
    padding: 10,
    margin: 5,
    borderRadius: 5,
    borderWidth: 1,
    borderColor: "black",
  },
  disabled: {
    backgroundColor: "#eee",
    borderColor: "#aaa",
  },
  disabledText: {
    color: "#aaa"
  }
});

export default buttonWithBackground;
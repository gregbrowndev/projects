import React from 'react';
import {TextInput, StyleSheet} from 'react-native';

const defaultInput = props => {
  const validStyle = !props.valid && props.touched ? styles.invalid : null;
  return (
    <TextInput
      underlineColorAndroid="transparent"
      {...props}
      style={[styles.input, props.style, validStyle]}
    />
  )
};

const styles = StyleSheet.create({
  input: {
    width: "100%",
    borderWidth: 1,
    borderColor: "#eee",
    padding: 5,
    marginTop: 8,
    marginBottom: 8
  },
  invalid: {
    backgroundColor: "#f9c0c0",
    borderColor: "red"
  }
});

export default defaultInput;
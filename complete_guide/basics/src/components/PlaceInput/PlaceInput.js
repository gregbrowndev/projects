import React from 'react';
import {View, TextInput, StyleSheet, Button} from 'react-native';

const placeInput = (props) => (
  <View style={styles.inputContainer}>
    <TextInput
      style={styles.placeInput}
      placeholder="An awesome place"
      value={props.placeName}
      onChangeText={props.changed}/>
    <Button
      styles={styles.placeButton}
      onPress={props.submitted}
      title="Add"/>
  </View>
);

const styles = StyleSheet.create({
  inputContainer: {
    // flex: 1,
    width: '100%',
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center'
  },
  placeInput: {
    width: '70%'
  },
  placeButton: {
    width: '30%'
  }
});

export default placeInput;
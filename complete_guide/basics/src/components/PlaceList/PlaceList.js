import React from 'react';
import {View, StyleSheet } from 'react-native';

import PlaceItem from './PlaceItem/PlaceItem';

const placeList = (props) => {
  const places = props.places
    .map((place, index) => <PlaceItem key={index} placeName={place}/>);

  return (
    <View style={styles.listContainer}>
      {places}
    </View>
  );
};

const styles = StyleSheet.create({
  listContainer: {
    width: '100%'
  }
});

export default placeList;
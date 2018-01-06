import React from 'react';
import {FlatList, StyleSheet} from 'react-native';

import PlaceItem from './PlaceItem/PlaceItem';

const placeList = (props) => (
  <FlatList
    style={styles.listContainer}
    data={props.places}
    renderItem={({item}) => (
      <PlaceItem
        placeName={item.name}
        onItemPressed={() => props.onItemDeleted(item.key)}
      />
    )}
  />
);

const styles = StyleSheet.create({
  listContainer: {
    width: '100%'
  }
});

export default placeList;
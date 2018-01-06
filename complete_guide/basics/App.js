import React from 'react';
import {StyleSheet, View} from 'react-native';

import PlaceInput from './src/components/PlaceInput/PlaceInput';
import PlaceList from './src/components/PlaceList/PlaceList';

export default class App extends React.Component {
  state={
    placeName: '',
    places: []
  };

  placeNameChangedHandler = (val) => {
    this.setState({
      placeName: val
    })
  };

  placeSumbitHandler = () => {
    if (this.state.placeName.trim() === '') {
      return;
    }

    this.setState(prevState => {
      return {
        places: prevState.places.concat(prevState.placeName)
      }
    })
  };

  render() {
    return (
      <View style={styles.container}>
        <PlaceInput
          changed={this.placeNameChangedHandler}
          submitted={this.placeSumbitHandler}
          placeName={this.state.placeName}
        />
        <PlaceList places={this.state.places} />
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    padding: 20,
    backgroundColor: '#fff',
    alignItems: 'center',
    justifyContent: 'flex-start',
  }
});

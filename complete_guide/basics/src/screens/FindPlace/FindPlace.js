import React, {Component} from 'react';
import {View, Text, TouchableOpacity, StyleSheet, Animated, Button} from 'react-native';
import {connect} from 'react-redux';

import PlaceList from '../../components/PlaceList/PlaceList';
import {getPlaces, removeToken} from '../../store/actions';


class FindPlaceScreen extends Component {

  state = {
    placesLoaded: false,
    fadeOutButton: new Animated.Value(1),
    fadeInList: new Animated.Value(0)
  };

  constructor(props) {
    super(props);
    console.log('constructor called');
    this.props.navigator.setOnNavigatorEvent(this.onNavigatorEvent);
  }

  componentDidMount() {
    console.log("FindPlaces did mount");
    this.props.getPlaces();
  }

  onNavigatorEvent = event => {
    // console.log(event);
    if (event.type === "NavBarButtonPress") {
      if (event.id === "sideDrawerToggle") {
        this.props.navigator.toggleDrawer({
          side: "left",
          animate: false
        });
      }
    }
  };

  itemSelectedHandler = key => {
    const selectedPlace = this.props.places.find(place => {
      return place.key === key;
    });

    this.props.navigator.push({
      screen: 'awesome-places.PlaceDetailScreen',
      title: selectedPlace.name,
      passProps: {
        selectedPlace: selectedPlace
      }
    });
  };

  placesSearchHandler = () => {
    Animated.timing(this.state.fadeOutButton, {
      toValue: 0,
      duration: 500,
      useNativeDriver: true
    }).start(this.placesLoadedHandler);
  };

  placesLoadedHandler = () => {
    this.setState({
      placesLoaded: true
    });
    Animated.timing(this.state.fadeInList, {
      toValue: 1,
      duration: 250,
      useNativeDriver: true
    }).start();
  };

  render() {
    if (this.state.placesLoaded) {
      return (
        <Animated.View style={{
          opacity: this.state.fadeInList,
          transform: [
            {
              translateY: this.state.fadeInList.interpolate({
                inputRange: [0, 1],
                outputRange: [150, 0]
              })
            },
          ]
        }}>
          <View>
            <Button
              title="Refresh Places"
              onPress={() => this.props.getPlaces()}
            />
          </View>
          <View>
            <Button
              title="Clear Token"
              onPress={() => this.props.removeToken()}
            />
          </View>
          <PlaceList
            places={this.props.places}
            onItemSelected={this.itemSelectedHandler}
          />
        </Animated.View>
      )
    } else {
      return (
        <View style={styles.buttonContainer}>
          <Animated.View style={{
            opacity: this.state.fadeOutButton,
            transform: [
              {
                scale: this.state.fadeOutButton.interpolate({
                  inputRange: [0, 1],
                  outputRange: [12, 1]
                })
              },
            ]
          }}>
            <TouchableOpacity onPress={this.placesSearchHandler}>
              <View style={styles.searchButton}>
                <Text style={styles.searchButtonText}>
                  Find Places
                </Text>
              </View>
            </TouchableOpacity>
          </Animated.View>
        </View>
      );
    }
  }
}

const styles = StyleSheet.create({
  buttonContainer: {
    flex: 1,
    justifyContent: "center",
    alignItems: "center"
  },
  searchButton: {
    borderColor: "orange",
    borderWidth: 3,
    borderRadius: 50,
    padding: 20
  },
  searchButtonText: {
    color: "orange",
    fontWeight: "bold",
    fontSize: 26
  }
});

const mapStateToProps = state => {
  return {
    places: state.places.places
  }
};

const mapDispatchToProps = dispatch => {
  return {
    getPlaces: () => dispatch(getPlaces()),
    removeToken: () => dispatch(removeToken())
  }
};

export default connect(mapStateToProps, mapDispatchToProps)(FindPlaceScreen);
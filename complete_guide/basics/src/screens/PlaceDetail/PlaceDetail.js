import React, {Component} from 'react';
import {Platform, View, Image, Text, StyleSheet, TouchableOpacity, ScrollView, Dimensions} from 'react-native';
import {connect} from 'react-redux';

import Icon from 'react-native-vector-icons/Ionicons';
import {deletePlace} from '../../store/actions';

class PlaceDetail extends Component {
  static navigatorStyle = {
    navBarButtonColor: "orange"
  };

  state = {
    viewMode: Dimensions.get('window').height > 500 ? "portrait" : "landscape"
  };

  constructor(props) {
    super(props);
    Dimensions.addEventListener("change", this.updateViewMode);
  }

  componentWillUnmount() {
    Dimensions.removeEventListener("change", this.updateViewMode)
  }

  updateViewMode = dims => {
    this.setState({
      viewMode: dims.window.height > 500 ? "portrait" : "landscape"
    });

    console.log('updateViewMode', this.state.viewMode);
    console.log('height', dims.height);
  };

  placeDeletedHandler = () => {
    this.props.onDeletePlace(this.props.selectedPlace.key);
    this.props.navigator.pop();
  };

  render() {
    return (
      <ScrollView>
        <View style={styles.container}>
          <View style={styles.headerContainer}>
            <Image
              source={this.props.selectedPlace.image}
              style={
                this.state.viewMode === "portrait"
                  ? styles.portraitPlaceImage
                  : styles.landscapePlaceImage
              }
            />
            <Text style={styles.placeName}>{this.props.selectedPlace.name}</Text>
          </View>
          <View>
            <TouchableOpacity onPress={this.placeDeletedHandler}>
              <View style={styles.deleteButton}>
                <Icon size={30} name={Platform.OS === 'android' ? 'md-trash' : 'ios-trash'} color="red"/>
              </View>
            </TouchableOpacity>
          </View>
        </View>
      </ScrollView>
    )
  }
}

const styles = StyleSheet.create({
  container: {
    margin: 22
  },
  headerContainer: {
    alignItems: 'center'
  },
  portraitPlaceImage: {
    width: "100%",
    height: 200
  },
  landscapePlaceImage: {
    width: "60%",
    height: 160
  },
  placeName: {
    fontWeight: "bold",
    textAlign: "center",
    fontSize: 28
  },
  deleteButton: {
    alignItems: 'center'
  }
});

const mapDispatchToProps = dispatch => {
  return {
    onDeletePlace: (key) => dispatch(deletePlace(key))
  };
};

export default connect(null, mapDispatchToProps)(PlaceDetail);
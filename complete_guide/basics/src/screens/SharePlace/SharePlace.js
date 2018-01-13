import React, {Component} from 'react';
import {View, Button, StyleSheet, ScrollView} from 'react-native';
import {connect} from 'react-redux';

import {addPlace} from '../../store/actions';
import MainText from '../../components/UI/MainText/MainText';
import HeadingText from '../../components/UI/HeadingText/HeadingText';
import ImagePickerWithPreview from '../../components/ImagePickerWithPreview/ImagePickerWithPreview';
import PlaceInput from '../../components/PlaceInput/PlaceInput';
import LocationPicker from '../../components/LocationPicker/LocationPicker';


class SharePlaceScreen extends Component {
  state = {
    placeName: '',
  };

  constructor(props) {
    super(props);
    console.log('constructor called');
    this.props.navigator.setOnNavigatorEvent(this.onNavigatorEvent);
  }

  onNavigatorEvent = event => {
    console.log(event);
    if (event.type === "NavBarButtonPress") {
      if (event.id === "sideDrawerToggle") {
        this.props.navigator.toggleDrawer({
          side: "left",
          animate: true
        });
      }
    }
  };

  placeNameChangedHandler = val => {
    this.setState({
      placeName: val
    });
  };

  placeAddedHandler = () => {
    const placeName = this.state.placeName.trim();
    if (placeName !== '') {
      this.props.onAddPlace(placeName);
    }
  };

  render() {
    return (
      <ScrollView>
        <View style={styles.container}>
          <MainText>
            <HeadingText>Share a Place with us!</HeadingText>
          </MainText>
          <ImagePickerWithPreview/>
          <LocationPicker/>
          <PlaceInput
            placeName={this.state.placeName}
            onChangeText={this.placeNameChangedHandler}
          />
          <View style={styles.button}>
            <Button
              title="Share the Place"
              onPress={this.placeAddedHandler}
            />
          </View>
        </View>
      </ScrollView>
    )
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'center',
  },
  button: {
    margin: 8
  },
});

const mapDispatchToProps = dispatch => {
  return {
    onAddPlace: (placeName) => dispatch(addPlace(placeName))
  }
};

export default connect(null, mapDispatchToProps)(SharePlaceScreen);
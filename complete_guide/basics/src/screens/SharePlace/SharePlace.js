import React, {Component} from 'react';
import {View, Button, StyleSheet, ScrollView} from 'react-native';
import {connect} from 'react-redux';

import {addPlace} from '../../store/actions';
import MainText from '../../components/UI/MainText/MainText';
import HeadingText from '../../components/UI/HeadingText/HeadingText';
import ImagePickerWithPreview from '../../components/ImagePickerWithPreview/ImagePickerWithPreview';
import PlaceInput from '../../components/PlaceInput/PlaceInput';
import LocationPicker from '../../components/LocationPicker/LocationPicker';
import validate from '../../utility/validation';


class SharePlaceScreen extends Component {
  state = {
    controls: {
      placeName: {
        value: "",
        valid: false,
        validationRules: {
          notEmpty: true
        },
        touched: false
      },
    }
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
    this.setState(prevState => {
      const prevControls = prevState.controls;

      // Get state of previous controls
      let controls = {
        ...prevControls,
      };

      // Now update state of control being changed
      const prevControl = prevControls.placeName;
      controls.placeName = {
        ...prevControl,
        value: val,
        valid: validate(val, prevControl.validationRules, null),
        touched: true
      };

      return {controls: controls};
    });
  };

  placeAddedHandler = () => {
    const placeName = this.state.controls.placeName.value.trim();
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
            placeName={this.state.controls.placeName.value}
            onChangeText={this.placeNameChangedHandler}
            valid={this.state.controls.placeName.valid}
            touched={this.state.controls.placeName.touched}
          />
          <View style={styles.button}>
            <Button
              title="Share the Place"
              onPress={this.placeAddedHandler}
              disabled={!this.state.controls.placeName.valid}
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
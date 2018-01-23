import React, {Component} from 'react';
import {
  View,
  Button,
  StyleSheet,
  ScrollView,
  ActivityIndicator
} from 'react-native';
import {connect} from 'react-redux';

import {addPlace} from '../../store/actions';
import MainText from '../../components/UI/MainText/MainText';
import HeadingText from '../../components/UI/HeadingText/HeadingText';
import ImagePickerWithPreview from '../../components/ImagePickerWithPreview/ImagePickerWithPreview';
import PlaceInput from '../../components/PlaceInput/PlaceInput';
import LocationPicker from '../../components/LocationPicker/LocationPicker';
import validate from '../../utility/validation';


class SharePlaceScreen extends Component {

  constructor(props) {
    super(props);
    console.log('constructor called');
    this.props.navigator.setOnNavigatorEvent(this.onNavigatorEvent);
  }

  componentWillMount() {
    this.reset();
  }

  reset() {
    this.setState({
      controls: {
        placeName: {
          value: "",
            valid: false,
            validationRules: {
            notEmpty: true
          },
          touched: false
        },
        location: {
          value: null,
            valid: false
        },
        image: {
          value: null,
            valid: false
        }
      }
    });
  }

  onNavigatorEvent = event => {
    // console.log(event);
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
    this.props.onAddPlace(
      this.state.controls.placeName.value,
      this.state.controls.location.value,
      this.state.controls.image.value
    );
  };

  locationPickHandler = location => {
    this.setState(prevState => {
      return {
        controls: {
          ...prevState.controls,
          location: {
            value: location,
            valid: true
          }
        }
      }
    });
  };

  imagePickHandler = image => {
    this.setState(prevState => {
      return {
        controls: {
          ...prevState.controls,
          image: {
            value: image,
            valid: true
          }
        }
      };
    });
    this.reset();
    this.imagePicker.reset();
    this.locationPicker.reset();
  };

  render() {
    let submitButton = (
      <Button
        title="Share the Place"
        onPress={this.placeAddedHandler}
        disabled={
          !this.state.controls.placeName.valid
          || !this.state.controls.location.valid
          || !this.state.controls.image.valid
        }
      />
    );

    if (this.props.isLoading) {
      submitButton = <ActivityIndicator/>;
    }

    return (
      <ScrollView>
        <View style={styles.container}>
          <MainText>
            <HeadingText>Share a Place with us!</HeadingText>
          </MainText>
          <ImagePickerWithPreview
            onImagePick={this.imagePickHandler}
            ref={ref => this.imagePicker = ref}
          />
          <LocationPicker
            onLocationPick={this.locationPickHandler}
            ref={ref => this.locationPicker = ref}
          />
          <PlaceInput
            placeName={this.state.controls.placeName.value}
            onChangeText={this.placeNameChangedHandler}
            valid={this.state.controls.placeName.valid}
            touched={this.state.controls.placeName.touched}
          />
          <View style={styles.button}>
            {submitButton}
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

const mapStateToProps = state => {
  return {
    isLoading: state.ui.isLoading
  }
};

const mapDispatchToProps = dispatch => {
  return {
    onAddPlace: (placeName, location, image) => dispatch(addPlace(placeName, location, image))
  }
};

export default connect(mapStateToProps, mapDispatchToProps)(SharePlaceScreen);
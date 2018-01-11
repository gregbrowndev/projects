import React from 'react';

import DefaultInput from '../UI/DefaultInput/DefaultInput';

class PlaceInput extends React.Component {
  state = {
    placeName: '',
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

    this.props.onPlaceAdded(this.state.placeName);
  };

  render() {
    return (
      <DefaultInput
        placeholder="Place Name"
        value={this.state.placeName}
        onChangeText={this.placeNameChangedHandler}
      />
    );
  }
}

export default PlaceInput;
import React from 'react';

const validation = (props) => {
  const input = props.input;
  if (input == null) {
    return null;
  }

  const inputLength = input.length;

  let message = "Valid";
  if (inputLength < 5) {
    message = "Invalid: Text too short!";
  } else if (inputLength > 10) {
    message = "Invalid: Text too long!";
  }

  return <p>{message}</p>;
};

export default validation;
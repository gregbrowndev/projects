import React from 'react';

const userInput = (props) => {
  const style = {
    width: '100%',
    padding: '12px 20px',
    margin: '8px 0',
    boxSizing: 'border-box'
  };

  return (
    <input type="text"
           style={style}
           onChange={props.changed}
           value={props.username}
           placeholder='Username'/>
  );
};

export default userInput;
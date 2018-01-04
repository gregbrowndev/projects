import React from 'react';

import styles from './Person.css';

const person = (props) => {
  return (
    <div className={styles.Person}>
      <p onClick={props.click}>I'm a {props.name} and I am {props.age} years old!</p>
      <p>{props.children}</p>
      <input type="text" onChange={props.changed} value={props.name}></input>
    </div>
  );

  // return [
  //     <p key="1" onClick={props.click}>I'm a {props.name} and I am {props.age} years old!</p>,
  //     <p key="2">{props.children}</p>,
  //     <input key="3" type="text" onChange={props.changed} value={props.name}></input>,
  // ];
};

export default person;
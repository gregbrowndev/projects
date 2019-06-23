import React from 'react';

import Aux from '../../hoc/Aux';
import styles from './Cockpit.css';

const cockpit = (props) => {
  let btnClass = styles.Button;
  if (props.showPersons) {
    btnClass = [styles.Button, styles.Red];
  }

  let classes = [];
  if (props.persons.length <= 2) {
    classes.push(styles.red);
  }
  if (props.persons.length <= 1) {
    classes.push(styles.bold);
  }

  return (
    <Aux>
      <h1>Hi, I'm a React App</h1>
      <p className={classes.join(' ')}>We're dynamically applying CSS classes</p>
      <button
        className={btnClass}
        onClick={props.clicked}>Toggle People
      </button>
    </Aux>
  );
};

export default cockpit;
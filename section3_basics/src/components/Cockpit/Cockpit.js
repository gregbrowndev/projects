import React from 'react';

import styles from './Cockpit.css';

const cockpit = (props) => {
  let btnClass = '';
  if (props.showPersons) {
    btnClass = styles.Red;
  }

  let classes = [];
  if (props.persons.length <= 2) {
    classes.push(styles.red);
  }
  if (props.persons.length <= 1) {
    classes.push(styles.bold);
  }

  return (
    <div className={styles.Cockpit}>
      <h1>Hi, I'm a React App</h1>
      <p className={classes.join(' ')}>We're dynamically applying CSS classes</p>
      <button
        className={btnClass}
        onClick={props.clicked}>Toggle People
      </button>
    </div>
  );
};

export default cockpit;
import React, {PureComponent} from 'react';

import styles from './App.css';
import Persons from "../components/Persons/Persons";
import Cockpit from "../components/Cockpit/Cockpit";

class App extends PureComponent {
  state = {
    persons: [
      {id: '1', name: 'Greg', age: 25},
      {id: '2', name: 'Max', age: 29},
      {id: '3', name: 'Manu', age: 27}
    ],
    showPersons: false
  };

  deletePersonHandler = (index) => {
    const persons = [...this.state.persons];
    persons.splice(index, 1);
    this.setState({persons: persons});
  };

  nameChangedHandler = (event, id) => {
    const personIndex = this.state.persons.findIndex(p => {
      return p.id === id;
    });

    const person = {...this.state.persons[personIndex]};

    person.name = event.target.value;

    const persons = [...this.state.persons];
    persons[personIndex] = person;

    this.setState({persons: persons});
  };

  render() {
    let persons = null;

    if (this.state.showPersons) {
      persons = <Persons clicked={this.deletePersonHandler}
                         changed={this.nameChangedHandler}
                         persons={this.state.persons}/>
    }

    return (
      <div className={styles.App}>
        <Cockpit showPersons={this.state.showPersons}
                 persons={this.state.persons}
                 clicked={this.togglePersonHandler}/>
        {persons}
      </div>
    );
  }

  togglePersonHandler = () => {
    this.setState({showPersons: !this.state.showPersons})
  }
}

export default App;

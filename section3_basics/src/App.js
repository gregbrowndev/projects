import React, {Component} from 'react';

import './App.css';
import Person from './Person/Person';

class App extends Component {
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
    const style = {
      backgroundColor: 'green',
      color: 'white',
      font: 'inherit',
      border: '1px solid blue',
      padding: '8px',
      cursor: 'pointer'
    };

    let persons = null;

    if (this.state.showPersons) {
      persons = (
        <div>
          {this.state.persons.map((person, index) => {
            return (
              <Person name={person.name}
                      age={person.age}
                      click={() => this.deletePersonHandler(index)}
                      key={person.id}
                      changed={(event) => this.nameChangedHandler(event, person.id)}
              >
              </Person>
            );
          })}
        </div>
      );

      style.backgroundColor = 'red';
    }

    let classes = [];
    if (this.state.persons.length <= 2) {
      classes.push('red');
    }
    if (this.state.persons.length <= 1) {
      classes.push('bold');
    }

    return (
        <div className="App">
          <h1>Hi, I'm a React App</h1>
          <p className={classes.join(' ')}>We're dynamically applying CSS classes</p>
          <button
            style={style}
            onClick={() => this.togglePersonHandler()}>Toggle People
          </button>
          {persons}
        </div>
    );
  }

  togglePersonHandler = () => {
    this.setState({showPersons: !this.state.showPersons})
  }
}

export default App;

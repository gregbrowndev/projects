import React, { Component } from 'react';
import './App.css';
import Person from './Person/Person';

class App extends Component {
  state = {
    persons: [
      { name: 'Greg', age: 25 },
      { name: 'Max', age: 29 },
      { name: 'Manu', age: 27 }
    ]
  }
  
  switchNameHandler = (name) => {
    console.log('Was clickeed!');
    this.setState({ 
      persons: [
        { name: name, age: 25 },
        { name: 'Max', age: 29 },
        { name: 'Manu', age: 27 }
      ]
    })
  }

  nameChangedHandler = (event) => {
    this.setState({ 
      persons: [
        { name: 'Greg', age: 25 },
        { name: 'Max', age: 29 },
        { name: event.target.value, age: 27 }
      ]
    })
  }

  render() {
    const style = {
      backgroundColor: 'white',
      font: 'inherit',
      border: '1px solid blue',
      padding: '8px',
      cursor: 'pointer'
    };

    return (
      <div className="App">
        <h1>Hi, I'm a React App</h1>
        <button 
          style={style}
          onClick={() => this.switchNameHandler('GREG')}>Switch Name</button>
        <Person 
          name={this.state.persons[0].name} 
          age={this.state.persons[0].age} 
          click={this.switchNameHandler.bind(this, 'greg!')}></Person>
          <Person name={this.state.persons[1].name} age={this.state.persons[1].age}></Person>
        <Person 
          name={this.state.persons[2].name} 
          age={this.state.persons[2].age} 
          changed={this.nameChangedHandler}>
        </Person>
      </div>
    );
  }
}

export default App;

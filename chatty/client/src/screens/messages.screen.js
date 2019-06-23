import { _ } from 'lodash';
import { ActivityIndicator, FlatList, KeyboardAvoidingView, StyleSheet, View, } from 'react-native';
import React, { Component } from 'react';
import PropTypes from 'prop-types';
import randomColor from 'randomcolor';
import { graphql, compose } from 'react-apollo';

import Message from '../components/message.component';
import MessageInput from '../components/message-input.component';
import CREATE_MESSAGE_MUTATION from '../graphql/create-message.mutation';
import GROUP_QUERY from '../graphql/group.query';

const styles = StyleSheet.create({
  container: {
    alignItems: 'stretch',
    backgroundColor: '#e5ddd5',
    flex: 1,
    flexDirection: 'column',
  },
  loading: {
    justifyContent: 'center'
  }
});

class Messages extends Component {
  static navigationOptions = ({ navigation }) => {
    const { state } = navigation;
    return {
      title: state.params.title,
    };
  };

  constructor(props) {
    super(props);
    const usernameColors = {};
    if (props.group && props.group.users) {
      props.group.users.forEach((user) => {
        usernameColors[user.username] = randomColor();
      })
    }

    this.state = {
      usernameColors
    };

    this.renderItem = this.renderItem.bind(this);
    this.send = this.send.bind(this);
  }

  componentWillReceiveProps(nextProps) {
    const usernameColors = {};
    // check for new messages
    if (nextProps.group) {
      if (nextProps.group.users) {
        // apply a color for each user
        nextProps.group.users.forEach((user) => {
          usernameColors[user.username] = this.state.usernameColors[user.username] || randomColor();
        });
      }

      this.setState({
        usernameColors
      });
    }
  }

  send(text) {
    this.props.createMessage({
      groupId: this.props.navigation.state.params.groupId,
      userId: 1,
      text
    }).then(() => {
      this.flatList.scrollToEnd({ animated: true });
    });
  }

  keyExtractor = item => item.id.toString();

  renderItem = ({item: message}) => (
    <Message
      color={this.state.usernameColors[message.from.username]}
      isCurrentUser={message.from.id === 1} // for now until we add auth
      message={message}/>
  );

  render() {
    const { loading, group } = this.props;

    // render loading placeholder
    if (loading && !group) {
      return (
        <View style={[styles.loading, styles.container]}>
          <ActivityIndicator/>
        </View>
      );
    }

    // render list of messages for group
    return (
      <KeyboardAvoidingView
        style={styles.container}
        behavior={'position'}
        contentContainerStyle={styles.container}
        keyboardVerticalOffset={64}
      >
        <FlatList
          ref={(ref) => { this.flatList = ref; }}
          data={group.messages.slice().reverse()}
          keyExtractor={this.keyExtractor}
          renderItem={this.renderItem}
          ListEmptyComponent={<View/>}
        />
        <MessageInput send={this.send}/>
      </KeyboardAvoidingView>
    );
  }
}

Messages.propTypes = {
  createMessage: PropTypes.func,
  navigation: PropTypes.shape({
    state: PropTypes.shape({
      params: PropTypes.shape({
        groupId: PropTypes.number
      })
    })
  }),
  group: PropTypes.shape({
    messages: PropTypes.array,
    users: PropTypes.array
  }),
  loading: PropTypes.bool
};

const groupQuery = graphql(GROUP_QUERY, {
  options: ownProps => ({
    variables: {
      groupId: ownProps.navigation.state.params.groupId
    }
  }),
  props: ({data: { loading, group }}) => ({
    loading, group
  })
});

const createMessageMutation = graphql(CREATE_MESSAGE_MUTATION, {
  props: ({ mutate }) => ({
    createMessage: ({ text, userId, groupId }) => mutate({
      variables: { text, userId, groupId },
      optimisticResponse: {
        __typename: 'Mutation',
        createMessage: {
          __typename: 'Message',
          id: -1,
          text,
          createdAt: new Date().toISOString(),
          from: {
            __typename: 'User',
            id: 1,
            username: 'Greg.Brown'
          },
          to: {
            __typename: 'Group',
            id: groupId
          }
        }
      },
      update: (store, { data: { createMessage }}) => {
        // Read the data from our cache for this query
        const groupData = store.readQuery({
          query: GROUP_QUERY,
          variables: {
            groupId
          }
        });

        // Add our message from the mutation to the end
        groupData.group.messages.unshift(createMessage);

        // Write our data back to the cache
        store.writeQuery({
          query: GROUP_QUERY,
          variables: {
            groupId
          },
          data: groupData
        });
      }
    })
  })
});

export default compose(
  groupQuery,
  createMessageMutation
)(Messages);
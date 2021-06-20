import React, { Component } from 'react';
import { ActivityIndicator, FlatList, StyleSheet, View, } from 'react-native';
import { _ } from 'lodash';
import PropTypes from 'prop-types';
import { graphql, compose } from 'react-apollo';

import Group from "../components/group.component";
import { USER_QUERY } from "../graphql/user.query";

const styles = StyleSheet.create({
  container: {
    backgroundColor: 'white',
    flex: 1,
  },
  loading: {
    justifyContent: 'center',
    flex: 1
  }
});

class Groups extends Component {
  static navigationOptions = {
    title: 'Chats',
  };

  constructor(props) {
    super(props);
    this.goToMessages = this.goToMessages.bind(this);
  }

  keyExtractor = item => item.id.toString();

  goToMessages(group) {
    const { navigate } = this.props.navigation;
    navigate('Messages', { groupId: group.id, title: group.name });
  }

  renderItem = ({ item }) => <Group group={item} goToMessages={this.goToMessages} />;

  render() {
    const { loading, user } = this.props;

    // render placeholder while loading
    if (loading) {
      return (
        <View style={[styles.loading, styles.container]}>
          <ActivityIndicator/>
        </View>
      )
    }

    // render list of groups for user
    return (
      <View style={styles.container}>
        <FlatList
          data={user.groups}
          keyExtractor={this.keyExtractor}
          renderItem={this.renderItem}
        />
      </View>
    );
  }
}

Groups.propTypes = {
  navigation: PropTypes.shape({
    navigate: PropTypes.func,
  }),
  data: PropTypes.shape({
    loading: PropTypes.bool,
    user: PropTypes.shape({
      id: PropTypes.number.isRequired,
      email: PropTypes.string.isRequired,
      groups: PropTypes.arrayOf(
        PropTypes.shape({
          id: PropTypes.number.isRequired,
          name: PropTypes.string.isRequired
        })
      )
    })
  })
};

// GraphQL query - used to wrap Groups component as a HOC
const userQuery = graphql(USER_QUERY, {
  options: (ownProps) => ({ variables: { id: 1 }}), // ownProps.id - hard coded to userId 1 in section 7 we add auth
  props: ({ data: { loading, user }}) => ({
    loading, user
  })
});

export default userQuery(Groups);
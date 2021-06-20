import React, {Component} from 'react';
import {Platform, View, Text, Dimensions, StyleSheet, TouchableOpacity} from 'react-native';
import {connect} from 'react-redux';
import Icon from 'react-native-vector-icons/Ionicons';
import {authLogout} from '../../store/actions';


class SideDrawer extends Component {
  render() {
    // Dimensions.get("window").width * 0.8
    return (
      <View style={[
        styles.container,
        {width: Dimensions.get("window").width * 0.8}
      ]}>
        <TouchableOpacity onPress={this.props.onLogout}>
          <View style={styles.drawerItem}>
            <Icon
              style={styles.drawerItemIcon}
              size={30}
              name={Platform.OS === 'android' ? 'md-log-out' : 'ios-log-out'}
              color="#aaa"
            />
            <Text>Logout</Text>
          </View>
        </TouchableOpacity>
      </View>
    );
  }
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    paddingTop: 50,
    backgroundColor: "white"
  },
  drawerItem: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 10,
    backgroundColor: "#eee"
  },
  drawerItemIcon: {
    marginRight: 10
  }
});


const mapDispatchToProps = dispatch => {
  return {
    onLogout: () => dispatch(authLogout())
  }
};

export default connect(null, mapDispatchToProps)(SideDrawer);
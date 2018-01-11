import React, {Component} from 'react';
import {View, Text, Dimensions, StyleSheet, TouchableOpacity} from 'react-native';
import Icon from 'react-native-vector-icons/Ionicons';


class SideDrawer extends Component {
  render() {
    // Dimensions.get("window").width * 0.8
    return (
      <View style={[
        styles.container,
        {width: Dimensions.get("window").width * 0.8}
      ]}>
        <TouchableOpacity>
          <View style={styles.drawerItem}>
            <Icon
              style={styles.drawerItemIcon}
              size={30}
              name="md-log-out"
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

export default SideDrawer;
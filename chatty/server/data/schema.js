export const Schema = [`
  # declare custom scalars
  scalar Date
  
  # a group chat entity
  type Group {
    id: Int! 
    name: String
    users: [User]!
    messages: [Message] 
  }
  
  # a user entity
  type User {
    id: Int!
    email: String!
    username: String!
    messages: [Message]
    groups: [Group]
    friends: [User]
  }
  
  # a message sent from a user to a group
  type Message {
     id: Int!
     to: Group!
     from: User!
     text: String!
     createdAt: Date!
  }
  
  type Query {
    # return a user by their id or email
    user(email: String, id: Int): User
    
    # return a group by its id
    group(id: Int!): Group
    
    # return messages sent by user or to group
    messages(groupId: Int, userId: Int): [Message]
  }
  
  schema {
      query: Query
  }`,
];

export default Schema;

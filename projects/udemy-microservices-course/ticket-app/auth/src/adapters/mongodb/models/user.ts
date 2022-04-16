import mongoose from 'mongoose';

// An interface that describes the User Document
interface UserDoc extends mongoose.Document {
  _id: string;
  email: string;
  password: string;
}

const userSchema = new mongoose.Schema<UserDoc>({
  _id: {
    type: String,
    required: true,
  },
  email: {
    type: String,
    required: true,
    index: true,
  },
  password: {
    type: String,
    required: true,
  },
});

const UserModel = mongoose.model<UserDoc>('User', userSchema);

export { UserModel, UserDoc };

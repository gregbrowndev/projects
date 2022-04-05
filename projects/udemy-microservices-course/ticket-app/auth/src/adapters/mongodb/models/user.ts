import mongoose from 'mongoose';

// An interface that describes the User Document
interface UserDoc extends mongoose.Document {
  _id: string;
  email: string;
  password: string;
}

const userSchema = new mongoose.Schema<UserDoc>(
  {
    _id: {
      type: String,
      required: true,
    },
    email: {
      type: String,
      required: true,
    },
    password: {
      type: String,
      required: true,
    },
  },
  {
    toJSON: {
      transform(doc, ret) {
        ret.id = ret._id.toHexString();
        delete ret._id;
        delete ret.password;
      },
      versionKey: false,
    },
  },
);

const UserModel = mongoose.model<UserDoc>('User', userSchema);

export { UserModel, UserDoc };

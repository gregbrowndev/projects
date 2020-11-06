import React, {useState} from "react";
import axios from 'axios';
import {BASE_URL} from './constants';

export default ({postId}) => {
    const [content, setContent] = useState('');

    const onSubmit = async (event) => {
          event.preventDefault();
          await axios.post(`${BASE_URL}/posts/${postId}/comments`, {
              content
          });
          setContent('');
    };

    return <div>
        <form onSubmit={onSubmit}>
            <div className="form-group">
                <label>New Comment</label>
                <input value={content} onChange={e => setContent(e.target.value)} type="text" className="form-control"/>
            </div>
            <button className="btn btn-primary">Submit</button>
        </form>
    </div>;
};

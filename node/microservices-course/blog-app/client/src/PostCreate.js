import React, {useState} from 'react';
import axios from 'axios';
import {BASE_URL} from './constants';

export default () => {
    const [title, setTitle] = useState('');

    const onSubmit = async (event) => {
        // prevent browser from submitting form itself
        event.preventDefault();

        // we use axios to post data to PostsService. We use async/await syntax instead of promises, callbacks, etc.
        await axios.post(`${BASE_URL}/posts/create`, {
            title
        });

        // clear form after submit
        setTitle("");
    };

    return <div>
        <form onSubmit={onSubmit}>
            <div className="form-group">
                <label>Title</label>
                <input value={title} onChange={e => setTitle(e.target.value)} type="text" className="form-control"/>
            </div>
            <button className="btn btn-primary">Submit</button>
        </form>
    </div>
};

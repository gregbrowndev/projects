import React, {useEffect, useState} from 'react';
import axios from 'axios';
import {BASE_URL} from './constants';
import CommentCreate from "./CommentCreate";
import CommentList from "./CommentList";

export default () => {
    const [posts, setPosts] = useState({});
    const fetchPosts = async () => {
        const res = await axios.get(`${BASE_URL}/posts`);
        console.log("[fetchPosts]", res.data);
        setPosts(res.data);
    };
    useEffect(() => {
        fetchPosts();
    }, []);

    console.log("[PostList] posts: ", posts)

    const listItems = Object.values(posts).map(p => {
        return <div className="card" style={{width: '30%', marginBottom: '20px'}} key={p.id}>
            <div className="card-body">
                <h3>{p.title}</h3>
                <CommentList comments={p.comments}/>
                <CommentCreate postId={p.id}/>
            </div>
        </div>
    })
    return <div className="d-flex flex-row flex-wrap justify-content-between">
        {listItems}
    </div>
};

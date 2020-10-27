import React from "react";

export default ({ comments }) => {
    console.log("[CommentList]", comments)
    const listItems = comments.map(c => {
        return <li key={c.id}>{c.content}</li>
    })

    return <ul>{listItems}</ul>;
};

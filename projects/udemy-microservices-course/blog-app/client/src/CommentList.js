import React from "react";

const getContent = (content, status) => {
    const contentMap = new Map([
        ["pending", "This comment is awaiting moderation"],
        ["rejected", "rejected"]
    ])
    return contentMap.get(status) || content;
}

export default ({ comments }) => {
    console.log("[CommentList]", comments)
    const listItems = comments.map(c => {
        const content = getContent(c.content, c.status)
        return <li key={c.id}>{content}</li>
    })

    return <ul>{listItems}</ul>;
};

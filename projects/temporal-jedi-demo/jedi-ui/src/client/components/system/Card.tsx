import React from 'react';

export interface CardProps {
  title: string;
  body?: React.ReactNode;
  footer?: React.ReactNode;
}
export const Card: React.FC<CardProps> = ({ title, body, footer }) => {
  return (
    <div className="bg-white px-4 py-4 shadow-lg sm:rounded-md md:px-8 md:py-8">
      {/* header*/}
      <div className="mb-4 md:mb-8">
        <h2 className="text-center text-xl md:text-4xl">{title}</h2>
      </div>

      {/* body */}
      <div className="mb-4 md:mb-8">{body}</div>

      {/* footer */}
      <div className="flex flex-row justify-center gap-4">{footer}</div>
    </div>
  );
};

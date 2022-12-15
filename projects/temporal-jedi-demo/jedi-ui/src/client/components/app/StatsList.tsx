import React from 'react';

export interface StatsListProps {
  items: React.ReactNode;
}

export const StatsList: React.FC<StatsListProps> = ({ items }) => {
  return <ul className="text-center text-lg md:mt-3 md:text-2xl">{items}</ul>;
};

import { useEffect, useRef, EffectCallback, DependencyList } from 'react';

export function useInterval(
  callback: EffectCallback,
  delay: number,
  deps?: DependencyList,
): void {
  const savedCallback = useRef<EffectCallback>();
  const finalDeps: DependencyList = [...(deps || []), delay];

  // Remember the latest callback.
  useEffect(() => {
    savedCallback.current = callback;
  }, [callback]);

  // Set up the interval.
  useEffect(() => {
    function tick() {
      if (savedCallback.current) {
        savedCallback.current();
      }
    }
    if (delay !== null) {
      let id = setInterval(tick, delay);
      return () => {
        clearInterval(id);
      };
    }
  }, finalDeps);
}

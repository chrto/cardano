import { useEffect, useRef } from 'react';

/**
 * A safe setInterval that:
 * - uses the latest callback (no stale closures)
 * - avoids overlapping executions (via internal lock)
 *
 * @param {Function} callback - The function to run
 * @param {number|null} delay - The interval time in ms; null to pause
 */
const useSafeInterval = (callback, delay) => {
  const callbackRef = useRef(callback);
  const isRunning = useRef(false);

  // Always keep latest callback in the ref
  useEffect(() => {
    callbackRef.current = callback;
  }, [callback]);

  useEffect(() => {
    if (delay == null) return;

    const tick = async () => {
      if (isRunning.current) return;
      isRunning.current = true;

      try {
        await callbackRef.current();
      } catch (err) {
        console.error('Error in useSafeInterval callback:', err);
      } finally {
        isRunning.current = false;
      }
    };

    const id = setInterval(tick, delay);
    return () => clearInterval(id);
  }, [delay]);
}

export default useSafeInterval;
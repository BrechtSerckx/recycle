export const serverUrl = new URL(
  process.env.REACT_APP_RECYCLE_ICS_SERVER_URL || window.location.origin
);
export const nodeEnv = process.env.NODE_ENV;

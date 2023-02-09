let LOGLEVEL = 50;

function log(level: number, ...args: string[]) {
  if (level <= LOGLEVEL) {
    // eslint-disable-next-line
    console.log(`[pitchy:${level}]`, ...args);
  }
}

function debug(...args: string[]) {
  log(50, ...args);
}

function info(...args: string[]) {
  log(30, ...args);
}

function warn(...args: string[]) {
  log(20, ...args);
}

function error(...args: string[]) {
  log(10, ...args);
}

function setLevel(level: number) {
  info('Setting log level to', level.toString());
  LOGLEVEL = level;
}

export {
  log, debug, info, warn, error, setLevel,
};
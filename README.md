## watch: live code reloading with watchman

`watch` is live code reloading application, inspired by `sync` [1].


### Features

 - Live-reload source code when changes.
 - Simple and efficient implementation by delegating heavy crunching to
   `watchman` [2].

### Platforms

 - OTP 17 and higher.
 - Linux, FreeBSD
 - MacOS is not supported yet because of lack of `stdbuf` command.


[1] https://github.com/rustyio/sync
[2] https://facebook.github.io/watchman

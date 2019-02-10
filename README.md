# Functional Differential Programming

## Build / Run

You need to have nginx installed. Starting a development server on
port 8080:

``` shell
nginx -p . -c nginx.conf
```

This will serve the contents of `./resources/public`. Refer to
`nginx.conf` for more details.

The server can be stopped like so:

``` shell
nginx -p . -s stop
```

Finally run `fdp.core` via figwheel or the ClojureScript REPL of your
choice and load `./resources/public/index.html`.

# Examples

## counter

This example demonstrates a *servant* server that holds a shared variable (using a `TVar`) and exposes an endpoint for reading its current value and another one for increasing its current value by 1.

In addition to that, it shows how you can generate the jquery-powered javascript functions corresponding to each endpoint, i.e one for reading the current value and one for increasing the value, and integrates all of that in a very simple HTML page. All these static files are served using the `serveDirectory` function from *servant*.

To see this all in action, simply run:

``` bash
$ cabal run counter
```

And point your browser to [http://localhost:8080/index.html](http://localhost:8080/index.html).

A copy of the generated docs is included in `counter.md` in this folder.
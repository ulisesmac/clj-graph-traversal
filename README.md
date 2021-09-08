# Graph Traversal 

A Clojure implementation for directed graphs

## Dependencies 

- Java 8 or Java 11
- Leiningen
- Clojure 1.10.1

## Execution

You can clone it and execute `lein repl` to start a REPL to try the functions or `lein run` and connect a remote REPL.

## Usage

You can see a graph example with `example-graph`.
The namespace `graph.core` comes with the next functions:

- `make-graph`
- `shortest-path`
- `eccentrycity`
- `radius`
- `diameter`

You can run `(doc function-name)` to see its usage.

Here is a brief example of execution:

```Clojure
(let [random-graph  (make-graph 10 20) ;; <- Random graph with  10 vertices and 20 directed edges 
      random-keys   (shuffle (keys random-graph))
      start-vertex  (first random-keys)
      target-vertex (last random-keys)]

  [random-graph
   start-vertex
   target-vertex
   (shortest-path random-graph start-vertex target-vertex)])
```

## Tests

This implementation comes with some tests to ensure that it behaves as expected, but they are not as rigorous as it 
should be, in fact they should verify properties instead of concrete cases. These tests have helped me to implement the
solution.

You can execute them with

```shell
lein test
```

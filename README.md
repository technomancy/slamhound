# Slamhound

    They sent a slamhound on Turner's trail in New Delhi, slotted it
    to his pheromones and the color of his hair. It caught up with him
    on a street called Chandni Chauk and came scrambling for his
    rented BMW through a forest of bare brown legs and pedicab
    tires. Its core was a kilogram of recrystallized hexogene and
    flaked TNT. He didn't see it coming. The last he saw of India was
    the pink stucco facade of a place called the Khush-Oil Hotel.

    Because he had a good agent, he had a good contract. Because he
    had a good contract, he was in Singapore an hour after the
    explosion. Most of him, anyway. The Dutch surgeon liked to joke
    about that, how an unspecified percentage of Turner hadn't made it
    out of Palam International on that first flight and had to spend
    the night there in a shed, in a support vat.

    It took the Dutchman and his team three months to put Turner
    together again. They cloned a square meter of skin for him, grew
    it on slabs of collagen and shark-cartilage polysaccharides. They
    bought eyes and genitals on the open market. The eyes were green.

    -- Count Zero, page 1. By William Gibson

Slamhound rips your ns form apart and reconstructs it. No Dutch
surgeon required.

Install it as a Leiningen plugin:

    $ lein plugin install slamhound 1.2.0

## Leiningen Usage

Take a namespace with a sparse ns form that won't compile:

    $ cat src/my/namespace.clj # before: ns form is missing clauses

    (ns my.namespace
      "I have a docstring.")

    (defn -main [& args]
      (pprint args)
      (io/copy (ByteArrayInputStream. (.getBytes "hello"))
               (first args))) 

Then run slamhound on it:

    $ lein slamhound src/my/namespace.clj # after: spits out new ns form

    (ns my.namespace
      "I have a doc string."
      (:use [clojure.pprint :only [pprint]])
      (:require [clojure.java.io :as io])
      (:import (java.io ByteArrayInputStream)))

For large projects, it can be slow to re-run from the command-line
since it has to load every namespace for every invocation. Leiningen's
interactive task is one way to mitigate this:

    $ lein int
    Welcome to Leiningen. Type help for a list of commands.
    lein> slamhound src/my/namespace.clj
    [...]

The first run will be slow, but successive runs will be quick.

## Repl Usage

You can do it manually from the repl too:

    user=> (use 'slam.hound)
    nil
    user=> (println (reconstruct "src/my/namespace.clj"))
    (ns my.namespace
      "I have a doc string."
      (:use [clojure.pprint :only [pprint]])
      (:require [clojure.java.io :as io])
      (:import (java.io ByteArrayInputStream)))

## Emacs Usage

The included `src/swank/payload/slamhound.el` allows for
convenient access within Slime sessions via `M-x slamhound` as well as
running it over an entire project with `M-x slamhound-project`.

You can install manually, but if you use `M-x clojure-jack-in` with
Swank Clojure 1.3.3 or newer to launch your Slime session then it will
be loaded into Emacs automatically.

Emacs version 24 or greater is required.

## The Rules

Slamhound can only rebuild your namespace if it follows the rules and
doesn't do anything too fancy. If your code depends upon a `:require`
clause, the required namespace must be aliased `:as` the last segment
of its name. Referring to fully-qualified vars is not supported. The
only supported option to `:use` is `:only`.

## Future Plans

* Better pretty-printing
* Allow for custom disambiguator functions

## License

Copyright © 2011 Phil Hagelberg

Distributed under the Eclipse Public License, the same as Clojure.

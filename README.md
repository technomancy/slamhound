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

Add `[slamhound "1.5.5"]` to the `:dependencies` of your `:user` profile.

## Screencast

<http://vimeo.com/80650659>

## Leiningen Usage

Make an alias for `run -m slam.hound` in your `:user` profile:

```clj
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}
```

Take a namespace with a sparse ns form that won't compile:

    $ cat src/my/namespace.clj # before: ns form is missing clauses

    (ns my.namespace
      "I have a doc string.")

    (defn -main [& args]
      (pprint args)
      (io/copy (ByteArrayInputStream. (.getBytes "hello"))
               (first args)))

Then run slamhound on it:

    $ lein slamhound src/my/namespace.clj # [... thinking ...]

    $ cat src/my/namespace.clj  # after: spits out new ns form
    (ns my.namespace
      "I have a doc string."
      (:require [clojure.java.io :as io]
                [clojure.pprint :refer [pprint]])
      (:import (java.io ByteArrayInputStream)))

Like magic.

Running on a directory will perform the same operation on every .clj file inside.

## Repl Usage

You can reconstruct namespaces from a repl to avoid the slow startup time:

    user=> (require '[slam.hound :refer [reconstruct]])
    nil
    user=> (println (reconstruct "src/my/namespace.clj"))
    (ns my.namespace
      "I have a doc string."
      (:require [clojure.java.io :as io]
                [clojure.pprint :refer [pprint]])
      (:import (java.io ByteArrayInputStream)))

Or to reconstruct files in place:

    user=> (slam.hound/swap-in-reconstructed-ns-form "src/my/namespace.clj")
    nil
    ;; Reload the file in your editor to pick up changes

## Emacs Usage

The included `slamhound.el` allows for convenient access within nREPL
or SLIME sessions via `M-x slamhound`. Install manually or via
[Marmalade](http://marmalade-repo.org).

## Vim Usage

Install [`vim-slamhound`](https://github.com/guns/vim-slamhound) for use
of the `:Slamhound` command within Clojure buffers.

## Light Table Usage

Install [`slamhound-lt`](https://github.com/chadhq/slamhound-lt) from the
central plugin repository.

## Shortcomings

### Syntax-quoted references

Slamhound will only find references in a namespace that are consumed
within the namespace itself. For example, if you have a macro that
refers to a var inside syntax-quote (backtick), but the macro is only
called from other namespaces, then Slamhound won't detect the reference
and will instead report the failure in the namespace in which the macro
is called.

You can work around this problem by attaching dummy metadata to the
`defmacro` form to prevent it from compiling without the necessary
references being present:

```clj
(defmacro defexample
  "Metadata can be attached to a macro by specifying an `attr-map`
  between the docstring and the params vector."
  {:requires [AClass a-var an/aliased-var #'a-macro]}
  [name & args]
  `(do (def ~(str name \*) (AClass. ~@args))
       (def ~(str name \-) (a-var ~@args))
       (def ~(str name \+) (an/aliased-var ~@args))
       (def ~(str name \!) (a-macro ~@args))))
```

Notice that macros must be referenced with `#'` to avoid a `Can't take
the value of a macro` error.

If the syntax quoted references refer to functions, classes, or constant
data, unquoting them will work as well:

```clj
(defmacro defexample
  {:requires [#'a-macro]}
  [name & args]
  `(do (def ~(str name \*) (new ~AClass ~@args))
       (def ~(str name \-) (~a-var ~@args))
       (def ~(str name \+) (~an/aliased-var ~@args))
       (def ~(str name \!) (a-macro ~@args))))
```

In this case `Aclass`, `a-var`, and `an/aliased-var` must be resolved at
compile time, so Slamhound will successfully find these references.

Note that the shorter reader macro form `(AClass. ~@args)` must be
expanded to `(new ~AClass ~@args)` and that macros cannot be unquoted.

### Fully qualified and dynamically resolved Vars

Slamhound will also not find references to fully-qualified vars or
vars resolved at runtime since it relies on detecting compilation
failures to determine when it's done.

### PermGen memory pressure

Slamhound aggressively creates and destroys namespaces during
reconstruction. This taxes the class loading system, so there is a
possibility that the JVM may run out of PermGen space in large projects.
If this happens, try running your JVM with the following option to
enable garbage collection of discarded classes:

```
-XX:+CMSClassUnloadingEnabled
```

### Side effects during namespace evaluation

As namespaces may potentially be reloaded many times during
reconstruction, it is important that no side-effects (aside from var
definition) occur during namespace evaluation.

## Leiningen 1.x

The `lein-slamhound` plugin is deprecated, and the `:aliases` approach
above is recommended for users of Leiningen 2. However, if you are
still using Leiningen 1.x you can use the `run` task:

    $ lein run -m slam.hound src/foo

Since Leiningen 1.x doesn't support partially-applied aliases, you
would have to make a shell alias if you don't want to type the full
invocation out every time.

## License

Copyright © 2011-2012 Phil Hagelberg and contributors

Distributed under the Eclipse Public License, the same as Clojure.

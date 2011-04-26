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

## Leiningen Usage

For this you will need to add Slamhound in both :dependencies and
:dev-dependencies:

    [slamhound "1.0.0"]

Then run from the command line:

    $ lein slamhound my.namespace

    (ns my.namespace
      "I have a doc string."
      (:use [clojure.pprint :only [pprint]])
      (:require [clojure.java.io :as io])
      (:import (java.io ByteArrayInputStream)))

## Emacs Usage

For this, it only needs to be in :dependencies. Once it's in, start a
Slime session. Then add this defun to your Emacs config:

    (defun slamhound ()
      (interactive)
      (goto-char (point-min))
      (kill-sexp)
      (insert (first (slime-eval `(swank:eval-and-grab-output
                                   (format "(do (require 'slam.hound)
                                              (slam.hound/reconstruct \"%s\"))"
                                           ,buffer-file-name))))))

Then you'll be able to run M-x slamhound to reconstruct your ns form.

## Future Plans

* Better pretty-printing
* Piggy-backing elisp inside jar
* Allow for custom disambiguator functions

## License

Copyright (C) 2011 Phil Hagelberg

Distributed under the Eclipse Public License, the same as Clojure.

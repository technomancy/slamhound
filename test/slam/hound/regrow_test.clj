(ns slam.hound.regrow-test
  {:slamhound-skip true}
  (:require [clojure.test :refer [deftest is testing]]
            [korma.core]
            [slam.hound.regrow :refer [candidates
                                       disambiguate grow-ns-map regrow]])
  (:refer-clojure :exclude [/]))

(def sample-body
  '((set/union #{:a} #{:b})
    (string/join ["a" "b"]) ; Try to conflict with set/join
    (UUID/randomUUID)
    (instance? Compiler$BodyExpr nil)
    (io/copy (ByteArrayInputStream. (.getBytes "remotely human"))
             (doto (File. "/tmp/remotely-human") .deleteOnExit))
    (deftest ^:unit test-ns-to-map
      (is (= (ns-from-map {:ns 'slam.hound}))))))

;; Classes and vars for testing
(defrecord RegrowTestRecord [])
(defrecord UUID [])
(def +i-must-be-a-cl-user+ true)
(def -+_$?!*><'' :horribly-named-var)
(def / :special-case-token)
(def CapitalVar true)
(def Pattern "Not java.util.Pattern")

(deftest ^:unit test-candidates
  (testing "finds static and dynamically created Java packages"
    (is (= (candidates :import 'UUID '((UUID/randomUUID)))
           '#{java.util.UUID}))
    (is (= (candidates :import 'Compiler$BodyExpr '(Compiler$BodyExpr))
           '#{clojure.lang.Compiler$BodyExpr}))
    (is (= (candidates :import 'RegrowTestRecord '((RegrowTestRecord.)))
           '#{slam.hound.regrow_test.RegrowTestRecord})))
  (testing "finds aliased namespaces"
    (is (= (candidates :alias 's '((s/join #{:a} #{:b})))
           '#{clojure.set clojure.string korma.core})))
  (testing "finds referred vars"
    (is (= (candidates :refer 'join '((join #{:a} #{:b})))
           '#{clojure.set clojure.string korma.core}))))

(deftest ^:unit test-disambiguate
  (testing "removes candidates matching disambiguator-blacklist"
    (is (nil? (disambiguate '#{swank lancet} :alias 'swank {}))))
  (testing "removes namespaces with excluded vars"
    (is (nil? (disambiguate '#{clojure.string clojure.set}
                            :refer 'join
                            '{:exclude {clojure.string #{join}
                                        clojure.set #{join}}})))
    (is (nil? (disambiguate '#{clojure.core core.logic} :refer '==
                            '{:xrefer #{clojure.core core.logic}
                              :refer {clojure.core #{} core.logic #{}}}))))
  (testing "prefers imports from old ns"
    (is (= (disambiguate '#{java.util.UUID slam.hound.regrow.UUID}
                         :import 'UUID
                         '{:import #{java.util.UUID}})
           '[:import java.util.UUID])))
  (testing "prefers aliases from old ns"
    (is (= (disambiguate '#{clojure.set clojure.string}
                         :alias 's
                         '{:alias {clojure.set s}})
           '[:alias clojure.set])))
  (testing "prefers refers from old ns"
    (is (= (disambiguate '#{clojure.set clojure.string}
                         :refer 'join
                         '{:refer {clojure.set #{join}}})
           '[:refer clojure.set])))
  (testing "prefers explicit refers over mass refers from old ns"
    (is (= (disambiguate '#{clojure.set clojure.string}
                         :refer 'join
                         '{:refer-all #{clojure.set}
                           :refer {clojure.string #{join}}})
           '[:refer clojure.string])))
  (testing "prefers aliases where the last segment matches"
    (is (= (disambiguate '#{clojure.set clojure.string} :alias 'set {})
           '[:alias clojure.set])))
  (testing "prefers shortest candidates when no other predicates match"
    (is (= (disambiguate '#{clojure.java.io clojure.set clojure.string}
                         :alias 'a {})
           '[:alias clojure.set])))
  (testing "changes type to :refer-all when top candidate is in old :refer-all"
    (is (= (disambiguate '#{clojure.set clojure.string}
                         :refer 'join
                         '{:refer-all #{clojure.set}})
           '[:refer-all clojure.set]))))

(deftest ^:unit test-grow-ns-map
  (testing "finds basic imports, aliases, and refers"
    (is (= (grow-ns-map {} :import 'RegrowTestRecord '((RegrowTestRecord.)))
           '{:import #{slam.hound.regrow_test.RegrowTestRecord}}))
    (is (= (grow-ns-map {} :alias 'string '((string/join)))
           '{:alias {clojure.string string}}))
    (is (= (grow-ns-map {} :refer 'pprint '((pprint [])))
           '{:refer {clojure.pprint #{pprint}}})))
  (testing "honors old :refer :all"
    (is (= (grow-ns-map '{:old {:refer-all #{clojure.pprint}}}
                        :refer 'pprint '((pprint [])))
           '{:old {:refer-all #{clojure.pprint}}
             :refer-all #{clojure.pprint}})))
  (testing "finds capitalized vars"
    (is (= (grow-ns-map '{} :refer 'CapitalVar '((type CapitalVar)))
           '{:refer {slam.hound.regrow-test #{CapitalVar}}}))))

(deftest ^:unit test-regrow
  (testing "prefers capitalized vars referred in old ns to classes"
    (is (= (regrow '[{:old {:refer {slam.hound.regrow-test #{Pattern}}}}
                     ((def p Pattern))])
           '{:old {:refer {slam.hound.regrow-test #{Pattern}}}
             :refer {slam.hound.regrow-test #{Pattern}}}))
    (is (= (regrow '[{:old {:refer {my.ns #{BitSet}}}}
                     ((def bs BitSet))])
           '{:old {:refer {my.ns #{BitSet}}}
             :import #{java.util.BitSet}})))
  (testing "finds referred vars with strange names"
    (is (= (regrow '[{} ((assert +i-must-be-a-cl-user+))])
           '{:refer {slam.hound.regrow-test #{+i-must-be-a-cl-user+}}}))
    (is (= (regrow '[{} ((keyword? -+_$?!*><''))])
           '{:refer {slam.hound.regrow-test #{-+_$?!*><''}}}))
    (is (= (regrow '[{:old {:exclude {clojure.core #{/} cljs.core #{/}}}
                      :exclude {clojure.core #{/}}}
                     ((keyword? /))])
           '{:old {:exclude {clojure.core #{/} cljs.core #{/}}}
             :exclude {clojure.core #{/}}
             :refer {slam.hound.regrow-test #{/}}}))))

(ns logical-interpreter)
(def parent-database "varon(juan).
	varon(pepe).
	varon(hector).
	varon(roberto).
	varon(alejandro).
	mujer(maria).
	mujer(cecilia).
	padre(juan, pepe).
	padre(juan, pepa).
	padre(hector, maria).
	padre(roberto, alejandro).
	padre(roberto, cecilia).
	hijo(X, Y) :- varon(X), padre(Y, X).
	hija(X, Y) :- mujer(X), padre(Y, X).
")

(defrecord Fact [name variables])
(defrecord Rule [name variables facts])

(defn remove_tabs_dots_and_white_spaces[rule_or_fact]
      (clojure.string/replace (clojure.string/replace rule_or_fact #"[\t.]" "") " " "")
      )

(defn get_variables [fact]
      (def variables (get (re-find #"\(([^\(]*)\)" fact) 1))
      (if (= nil variables)
        (throw (Exception. "Invalid Fact"))
        (clojure.string/split variables #",")
        )
      )

(defn get_name [fact]
      (def name (get (re-find #"([^\(]*)\(" fact) 1))
      (if (= nil name)
        (throw (Exception. "Invalid Fact"))
        name
        )
      )

(defn get_facts [string_rule]
      (def facts (get (re-find #":-(.*)" string_rule) 1))
      (if (= nil facts)
        (throw (Exception. "Invalid Rule"))
        )
      (map create_fact_or_rule (clojure.string/split facts #"(?<![A-Z]),"))
      )

(defn create_fact_or_rule [fact_or_rule]
      (if (= nil (re-find #":-" fact_or_rule))
        (new Fact (get_name fact_or_rule) (get_variables fact_or_rule))
        nil
        )
      )

(defn evaluate-query [database query]
      "Returns true if the rules and facts in database imply query, false if not. If
       either input can't be parsed, returns nil"
      (def list_facts_and_rules (clojure.string/split-lines database))
      (def list_facts_and_rules (map remove_tabs_dots_and_white_spaces list_facts_and_rules))
      (def list_facts_and_rules (map create_fact_or_rule list_facts_and_rules))
      (some #(= (create_fact_or_rule query) %) list_facts_and_rules)
      )

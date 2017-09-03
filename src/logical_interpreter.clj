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


(defn replace-map
      "given an input string and a hash-map, returns a new string with all
      keys in map found in input replaced with the value of the key"
      [s m]
      (clojure.string/replace s
                              (re-pattern (apply str (interpose "|" (map #(java.util.regex.Pattern/quote %) (keys m)))))
                              m))


(defn get_facts_list [facts_string variables_map]
      (def splited_facts (clojure.string/split facts_string #"(?<![A-Z]),"))
      (into [] (map create_fact_or_rule (map #(replace-map % variables_map ) splited_facts))
            ))


(defrecord Fact [name variables])

(defrecord Rule [name variables facts])

(defmulti match_to (fn [this other] [(class this) (class other)]))

(defmethod match_to [Fact Fact] [this other] (= this other))

(defmethod match_to [Fact Rule] [this other]
           (if (not= (:name this) (:name other))
             false
             (if (not= (count (:variables this)) (count (:variables other)))
               false
               (are_all_facts_true this other)
               )
             )
           )

(defn is_the_fact_true [fact list_of_facts_and_rules]
      (if (= nil (some #(match_to fact %) list_of_facts_and_rules))
        false
        true
        )
      )

(defn are_all_facts_true [fact rule]
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (def database "varon(juan).
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
  	hija(X, Y) :- mujer(X), padre(Y, X).")
      (def list_facts_and_rules (clojure.string/split-lines database))
      (def list_facts_and_rules (map remove_tabs_dots_and_white_spaces list_facts_and_rules))
      (def list_facts_and_rules (map create_fact_or_rule list_facts_and_rules))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (def facts (get_facts_list (:facts rule) (zipmap (:variables rule) (:variables fact))))
      (every? #(= true %) (map #(is_the_fact_true % list_facts_and_rules) facts))
      )

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

(defn get_facts_string [string_rule]
      (def facts (get (re-find #":-(.*)" string_rule) 1))
      (if (= nil facts)
        (throw (Exception. "Invalid Rule"))
        )
      (doall (map create_fact_or_rule (clojure.string/split facts #"(?<![A-Z]),")))
      facts
      )

(defn create_fact_or_rule [fact_or_rule]
      (if (= nil (re-find #":-" fact_or_rule))
        (new Fact (get_name fact_or_rule) (get_variables fact_or_rule))
        (new Rule (get_name fact_or_rule) (get_variables fact_or_rule) (get_facts_string fact_or_rule))
        )
      )

(defn evaluate-query [database query]
      "Returns true if the rules and facts in database imply query, false if not. If
       either input can't be parsed, returns nil"
      (def list_facts_and_rules (clojure.string/split-lines database))
      (def list_facts_and_rules (map remove_tabs_dots_and_white_spaces list_facts_and_rules))
      (def list_facts_and_rules (map create_fact_or_rule list_facts_and_rules))
      (if (= nil (some #(match_to (create_fact_or_rule query) %) list_facts_and_rules))
        false
        true
        )
      )

;(evaluate-query parent-database "hijo(pepe, juan)")
(def factstring "hija(cecilia,roberto)")
(def fact (new Fact (get_name factstring) (get_variables factstring)))
(def rulestring "hija(X,Y):-mujer(X),padre(Y,X)")
(def rule (new Rule(get_name rulestring) (get_variables rulestring) (get_facts_string rulestring)))


(are_all_facts_true fact rule)
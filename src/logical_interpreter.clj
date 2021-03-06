(ns logical-interpreter)

(defn replace-map
      "given an input string and a hash-map, returns a new string with all
      keys in map found in input replaced with the value of the key"
      [s m]
      (clojure.string/replace s
                              (re-pattern (apply str (interpose "|" (map #(java.util.regex.Pattern/quote %) (keys m)))))
                              m))

(defrecord Fact [name variables])

(defrecord Rule [name variables facts])

(defn get_name [fact]
      (def fact_name (get (re-find #"([^\(]*)\(" fact) 1))
      (if (= nil fact_name)
        (throw (Exception. "Invalid Fact"))
        fact_name
        )
      )
(declare get_facts_string)
(declare get_variables)
(declare are_all_facts_true)

(defn create_fact_or_rule [fact_or_rule]
      (if (= nil (re-find #":-" fact_or_rule))
        (new Fact (get_name fact_or_rule) (get_variables fact_or_rule))
        (new Rule (get_name fact_or_rule) (get_variables fact_or_rule) (get_facts_string fact_or_rule))
        )
      )

(defn get_facts_list [facts_string variables_map]
      (def splited_facts (clojure.string/split facts_string #"(?<![A-Z]),"))
      (into [] (map create_fact_or_rule (map #(replace-map % variables_map ) splited_facts))
            ))

(defmulti match (fn [this other db] [(class this) (class other) (class db)]))

(defmethod match [Fact Fact clojure.lang.PersistentVector] [this other db] (= this other))

(defmethod match [Fact Rule clojure.lang.PersistentVector] [this other db]
           (if (not= (:name this) (:name other))
             false
             (if (not= (count (:variables this)) (count (:variables other)))
               false
               (are_all_facts_true this other db)
               )
             )
           )

(defn is_the_fact_true [fact list_of_facts_and_rules]
      (if (= nil (some #(match fact % []) list_of_facts_and_rules))
        false
        true
        )
      )

(defn are_all_facts_true [fact rule db]
      (def facts (get_facts_list (:facts rule) (zipmap (:variables rule) (:variables fact))))
      (every? #(= true %) (map #(is_the_fact_true % db) facts))
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


(defn get_facts_string [string_rule]
      (def facts (get (re-find #":-(.*)" string_rule) 1))
      (if (= nil facts)
        (throw (Exception. "Invalid Rule"))
        )
      (doall (map create_fact_or_rule (clojure.string/split facts #"(?<![A-Z]),")))
      facts
      )

(defn evaluate-query [database query]
      "Returns true if the rules and facts in database imply query, false if not. If
       either input can't be parsed, returns nil"
      (try
        (def list_facts_and_rules (into [] (remove empty? (clojure.string/split-lines database))))
        (def list_facts_and_rules (map remove_tabs_dots_and_white_spaces list_facts_and_rules))
        (def list_facts_and_rules  (into [] (doall (map create_fact_or_rule list_facts_and_rules))))
        (if (= nil (some #(match (create_fact_or_rule (remove_tabs_dots_and_white_spaces query)) % list_facts_and_rules) list_facts_and_rules))
          false
          true
          )
        (catch Exception e)
        )
      )
(ns fhirpath-query-generator.core
  (require [clojure.math.combinatorics :as combo]
           [clojure.string :as s])
  (:gen-class))

(defn gen-add-opr [left-expr right-expr]
  {:type :addition
   :operator "+"
   :left-expr left-expr
   :right-expr right-expr})

(defn gen-str [len]
  {:type :string
   :value (apply str (take len (repeatedly #(char (+ (rand 26) 65)))))})

(defn gen-bool
  ([] (gen-bool (if (= (rand-int 2) 1) "true" "false")))
  ([value] {:type :boolean :value (str value)}))

(defn gen-int
  ([]
   (let [abs-range 100]
     (gen-int (- (rand-int (* abs-range 2)) abs-range))))
  ([value] {:type :integer :value value}))

(def simple-expressions [{:type :string :value "wow"}
                         {:type :ident :value "name"}
                         (gen-int 0)
                         (gen-int -1)
                         (gen-int 1)
                         (gen-bool true)
                         (gen-bool false)
                         #_(gen-add-opr (gen-int -100) (gen-int 100))])

(def iif-spec
  {:name "iif"
   :args [{:type :expression}
          {:type :expression
           :optional true}
          {:type :expression
           :optional true}
          {:type :expression
           :optional true}]})

(defn gen-args [arg-spec]
  (if (:optional arg-spec)
    (conj simple-expressions nil)
    simple-expressions))

(defn gen-valid-funcs [input-expr spec]
  (let [args (mapv #(remove nil? %)
                   (apply combo/cartesian-product
                          (map (fn [arg-spec] (gen-args arg-spec))
                               (:args spec))))]
    (mapv (fn [arg] {:type :function
                     :name (:name spec)
                     :input-expr input-expr
                     :args arg}) args)))

(comment
  (gen-valid-funcs "Patient" iif-spec))

(def h
  (-> (make-hierarchy)
      (derive :literal :expression)
      (derive :function :expression)
      (derive :operator :expression)
      (derive :ident :expression)
      (derive :index :expression)
      (derive :string :literal)
      (derive :boolean :literal)
      (derive :integer :literal)
      (derive :addition :operator)))

#_(def fhirpath-query-str nil)
(defmulti fhirpath-query-str :type :hierarchy #'h)

(defmethod fhirpath-query-str :literal [expr] (:value expr))
(defmethod fhirpath-query-str :string [expr] (str "'" (:value expr) "'"))
(defmethod fhirpath-query-str :ident [expr] (:value expr))
(defmethod fhirpath-query-str :operator [expr]
  (str "("
       (fhirpath-query-str (:left-expr expr))
       " "
       (:operator expr)
       " "
       (fhirpath-query-str (:right-expr expr))
       ")"))

(defmethod fhirpath-query-str :function [func-expr]
  ; (println (:input-expr func-expr))
  ; (println (fhirpath-query-str (:input-expr func-expr)))
  ; (println (:args func-expr))
  ; (println (map :value (:args func-expr)))
  (str (fhirpath-query-str (:input-expr func-expr))
       "."
       (:name func-expr)
       "("
       (s/join ", " (map fhirpath-query-str (:args func-expr)))
       ")"))

; (fhirpath-query-str {:type :operator :left-expr {:type :string :value "ws"} :operator "-" :right-expr {:type :operator :left-expr {:type :integer :value 1} :operator "*" :right-expr {:type :integer :value 2}}})

(defn -main [& args]
  (println (s/join "\n" (map fhirpath-query-str (gen-valid-funcs {:type :ident :value "Root"} iif-spec)))))

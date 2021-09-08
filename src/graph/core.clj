(ns graph.core
  (:require
   [graph.impl :as impl]))


(def example-graph
  "A graph example of the representation for this functions.
  A graph is a hashmap where:
    - key is a vertex in the graph
    - value is a vector of adjacencies for that vertex.
  Each adjacency is a map with its `:vertex` and its edge's `:weight`."
  {:A [{:vertex :B
        :weight 1}
       {:vertex :C
        :weight 2}],
   :B [{:vertex :D
        :weight 1}],
   :C [{:vertex :D
        :weight 1}],
   :D [{:vertex :E
        :weight 1}]
   :E []}) ;; vertices are letters due to readability but can be keywords with numbers


(defn make-graph
  "Creates a random graph with `n` vertices and `s` sparseness.
  with `s` such that:
   n-1 <= s <= n * (n - 1)."
  ([n s]
   (let [min-connected-graph (impl/create-random-graph-connected n)
         has-max-degree?     #(= (count %) (- n 1))
         all-vertices        (keys min-connected-graph)]
     (reduce (fn [graph _]
               (let [[vertex adjs] (->> graph vec shuffle (into {})
                                        (filter #(-> (second %) has-max-degree? not))
                                        first)
                     valid-vertices (->> all-vertices
                                         (remove (set (conj (map :vertex adjs) vertex)))
                                         first)]
                 (update graph vertex conj {:vertex valid-vertices
                                            :weight (rand-int impl/max-rand-weight)})))
             min-connected-graph
             (range (- s (- n 1)))))))


(defn- recreate-shortest-path
  "Receives a map of relations produced when `shortest-path` ends its recursion
  and returns the shortest path or `:no-path`."
  [paths target]
  (let [result ((fn [p current acc]
                  (if (nil? (current p))
                    (conj acc current)
                    (recur p (current p) (conj acc current))))
                paths target '())]
    (if (= 1 (count result))
      :no-path
      result)))


(defn- get-weight
  "Returns the weight of the adjacent vertex `adj-vertex` to `vertex`
  in a `graph`."
  [graph vertex adj-vertex]
  (:weight (first (filter #(= (:vertex %) adj-vertex)
                          (vertex graph)))))


(defn shortest-path
  "Implementation of Dijkstra's algorithm to find the shortest path between two vertices.
  `graph` must be a weighted graph
  - `v-start` and `target` must be keyword names of vertices in `graph`."
  ([graph v-start target]
   (let [vertices (-> (zipmap (keys graph)
                              (repeat {:weight   ##Inf
                                       :visited? false}))
                      (update v-start assoc :weight 0))
         paths     (zipmap (keys graph)
                          (repeat nil))]
     (shortest-path graph v-start target vertices paths)))
  ([graph v-current-name target remaining-vertices paths]
   (if (= v-current-name target)
     (recreate-shortest-path paths target)
     (let [neighbors               (set (map :vertex (v-current-name graph)))
           v-current               (v-current-name remaining-vertices)
           remaining-vertices      (update remaining-vertices v-current-name assoc :visited? true)
           ;;
           {:keys [updated-weights
                   updated-paths]} (reduce (fn [acc [neighbor-name {visited? :visited?, stored-weight :weight :as props}]]
                                             (if (and (neighbors neighbor-name)
                                                      (not visited?))
                                               (let [acc-weight (+ (:weight v-current)
                                                                   (get-weight graph v-current-name neighbor-name))]
                                                 (if (< acc-weight stored-weight)
                                                   (-> acc
                                                       (assoc-in [:updated-weights neighbor-name] (assoc props :weight acc-weight))
                                                       (assoc-in [:updated-paths neighbor-name] v-current-name))
                                                   acc))
                                               acc))
                                           {:updated-weights remaining-vertices
                                            :updated-paths   paths}
                                           remaining-vertices)

           sorted-weights          (sort-by (fn [[_ {:keys [weight visited?]}]]
                                              (if visited? ##Inf weight))
                                            updated-weights)
           [[v-next & _] & _]      (remove #(:visited? (second %)) sorted-weights)]

       (recur graph v-next target (into {} sorted-weights) updated-paths)))))


(defn eccentricity
  "Calculates the eccentricity of `vertex` in `graph`.
  A `vertex` is a keyword."
  [graph vertex]
  (let [rest-of-vertices (remove #{vertex} (keys graph))
        distances        (->> rest-of-vertices
                              (map (partial shortest-path graph vertex))
                              (map #(when (seq? %)
                                      (count %)))
                              (filter some?)
                              (map dec)
                              (sort >))]
    (nth distances 0 0)))


(defn- eccentricity*
  "Calculates all eccentricities of vertices in `graph`, then sorts
  the result by `op` and returns the first item."
  [graph op]
  (let [vertices       (keys graph)
        eccentricities (->> vertices
                            (map (partial eccentricity graph))
                            (filter some?)
                            (sort op))]
    (first eccentricities)))


(defn radius
  "Returns the radius of `graph`."
  [graph]
  (eccentricity* graph <))


(defn diameter
  "Returns the diameter of `graph`."
  [graph]
  (eccentricity* graph >))


(ns graph.core-test
  (:require
   [clojure.test :refer :all]
   [graph.core :as sut]
   [graph.impl :as impl]))


(defn- visit-vertices
  ([graph]
   (visit-vertices graph (first (first graph))))
  ([graph current-vertex]
   (let [adjs (current-vertex graph)]
     (if (empty? adjs)
       []
       (cons current-vertex (mapcat (fn [vertex]
                                      (visit-vertices (dissoc graph current-vertex) vertex))
                                    adjs))))))


(defn- ->undirected-graph [graph]
  (if (= 1 (count graph))
    graph
    (reduce (fn [acc [k adjs]]
              (reduce (fn [acc2 v2]
                        (let [k (first (first v2))
                              v (second (first v2))]
                          (update acc2 k (fn [viejo]
                                           (vec (concat viejo v))))))
                      acc
                      (mapcat (fn [{:keys [vertex]}]
                                (when vertex
                                  [{vertex [k]}
                                   {k [vertex]}]))
                              adjs)))
            {}
            graph)))


(defn- is-connected?
  [graph]
  (if (= 1 (count graph))
    true
    (let [vertices         (keys graph)
          vertices-visited (visit-vertices graph)]

      (and (= (count vertices) (count vertices-visited))
           (= (set vertices) (set vertices-visited))))))


(deftest create-random-tree-test ;; not a serious test
  (testing "Creates a random tree OK"
    (testing "of order 1"
      (is (= {:A []} (impl/create-random-graph-connected 1))))

    (testing "of order 2"
      (let [graph (impl/create-random-graph-connected 2)]

        (is (= #{:A :B} (set (keys graph))))
        (are [pred] (some pred graph)
             #(empty? (second %))
             #(seq (second %)))))

    (testing "is connected"
      (are [n] (-> (impl/create-random-graph-connected n)
                   (->undirected-graph)
                   (is-connected?))
           1 5 10 100 1000 10000 20000))))


(deftest make-graph-test
  (testing "Properties of simple directed graphs"
    (testing "Does not have loops."

      (let [has-loops?     (fn [[vertex adjs]]
                             ((set (map :vertex adjs)) vertex))
            random-order-1 (inc (rand-int 10000))
            random-order-2 (inc (rand-int 200))
            random-order-3 (inc (rand-int 200))]
        ;; minimum number of edges
        (are [num-vertices] (every? #(not (has-loops? %))
                                    (sut/make-graph num-vertices (- num-vertices 1)))
             2
             10
             20
             100
             200
             1000
             10000
             random-order-1)
        ;; complete graph
        (are [num-vertices] (every? #(not (has-loops? %))
                                    (sut/make-graph num-vertices (* num-vertices (- num-vertices 1))))
             2
             10
             20
             100
             200
             random-order-2)

        ;; a graph with size between n-1 and (n * (n-1))
        (are [num-vertices] (every? #(not (has-loops? %))
                                    (sut/make-graph num-vertices (+ num-vertices 1
                                                                    (rand-int (- (* num-vertices (- num-vertices 1))
                                                                                 num-vertices)))))
             2
             10
             20
             100
             200
             random-order-3)))

    (testing "There are not multiple arrows with same source and target nodes."

      (let [not-repeated-edges? (fn [[_ adjs]]
                                  (= (count (set adjs)) (count adjs)))
            random-order-1 (inc (rand-int 10000))
            random-order-2 (inc (rand-int 200))
            random-order-3 (inc (rand-int 200))]

        ;; minimum number of edges
        (are [num-vertices] (every? #(not-repeated-edges? %)
                                    (sut/make-graph num-vertices (- num-vertices 1)))
             2
             10
             20
             100
             200
             1000
             10000
             random-order-1)
        ;; complete graph
        (are [num-vertices] (every? #(not-repeated-edges? %)
                                    (sut/make-graph num-vertices (* num-vertices (- num-vertices 1))))
             2
             10
             20
             100
             200
             random-order-2)

        ;; a graph with size between n-1 and (n * (n-1))
        (are [num-vertices] (every? #(not-repeated-edges? %)
                                    (sut/make-graph num-vertices (+ num-vertices 1
                                                                    (rand-int (- (* num-vertices (- num-vertices 1))
                                                                                 num-vertices)))))
             2
             10
             20
             100
             200
             random-order-3)))))


(deftest shortest-path-test
  (testing "Shortest path for two vertices"
    (let [graph {:A [{:vertex :B
                      :weight 6}]
                 :B []}]

      (is (= [:A :B] (sut/shortest-path graph :A :B)))
      (is (= :no-path (sut/shortest-path graph :B :A))))

    (testing "Shortest path before last vertex"
      (let [graph {:A [{:vertex :B
                        :weight 6}]
                   :B [{:vertex :C
                        :weight 2}]
                   :C [{:vertex :D
                        :weight 3}]
                   :D []}]

        (is (= [:A :B :C] (sut/shortest-path graph :A :C)))
        (is (= [:A :B :C :D] (sut/shortest-path graph :A :D)))
        (is (= :no-path (sut/shortest-path graph :D :A)))
        (is (= :no-path (sut/shortest-path graph :B :A)))))

    (testing "Shortest path with two options"
      (let [graph {:A [{:vertex :B
                        :weight 1}]
                   :B [{:vertex :X
                        :weight 1}
                       {:vertex :C
                        :weight 2}]
                   :X [{:vertex :D
                        :weight 1}]
                   :C [{:vertex :D
                        :weight 1}]
                   :D []}]

        (is (= [:A :B :X :D] (sut/shortest-path graph :A :D)))))

    (testing "Path that becomes cheaper if first takes the expensive path"
      (let [graph {:A [{:vertex :B
                        :weight 1}]
                   :B [{:vertex :X
                        :weight 1}
                       {:vertex :C
                        :weight 1}]
                   :C [{:vertex :D
                        :weight 1}]
                   :D [{:vertex :E
                        :weight 100}]
                   :X [{:vertex :Y
                        :weight 10}]
                   :Y [{:vertex :E
                        :weight 10}]
                   :E []}]

        (is (= [:A :B :X :Y :E] (sut/shortest-path graph :A :E))))

      (let [graph {:A [{:vertex :B
                        :weight 1}]
                   :B [{:vertex :X
                        :weight 10}
                       {:vertex :C
                        :weight 1}]
                   :C [{:vertex :D
                        :weight 1}]
                   :D [{:vertex :E
                        :weight 100}]
                   :X [{:vertex :Y
                        :weight 10}]
                   :Y [{:vertex :E
                        :weight 10}]
                   :E []}]

        (is (= [:A :B :X :Y :E] (sut/shortest-path graph :A :E)))))

    (testing "Unreachable target"

      (let [graph {:A [{:vertex :B
                        :weight 6}]
                   :B []}]

        (is (= :no-path (sut/shortest-path graph :B :A))))

      (let [graph {:A [{:vertex :B
                        :weight 1}]
                   :B [{:vertex :X
                        :weight 10}
                       {:vertex :C
                        :weight 1}]
                   :C [{:vertex :D
                        :weight 1}]
                   :D [{:vertex :A
                        :weight 100}]
                   :X [{:vertex :Y
                        :weight 10}]
                   :Y [{:vertex :B
                        :weight 10}]
                   :Z []}]

        (is (= :no-path (sut/shortest-path graph :A :Z))))

      (let [graph {:A [{:vertex :B
                        :weight 6}]
                   :B [{:vertex :A
                        :weight 5}]
                   :Z []}]

        (is (= :no-path (sut/shortest-path graph :A :Z)))
        (is (= :no-path (sut/shortest-path graph :B :Z)))))

    (testing "Complex routes that return to origin"
      (let [graph {:A [{:vertex :B
                        :weight 1}
                       {:vertex :C
                        :weight 1}
                       {:vertex :D
                        :weight 1}]
                   :B [{:vertex :A
                        :weight 10}]
                   :C [{:vertex :A
                        :weight 1}]
                   :D [{:vertex :A
                        :weight 15}
                       {:vertex :X
                        :weight 150}]
                   :X [{:vertex :A
                        :weight 10}
                       {:vertex :Y
                        :weight 100}
                       {:vertex :Z
                        :weight 100}]
                   :Y [{:vertex :B
                        :weight 10}]
                   :Z []}]

        (is (= [:B :A :D :X :Z] (sut/shortest-path graph :B :Z)))))))


(deftest eccentricity-test
  (testing "Eccentricity"
    (let [graph {:A [{:vertex :B
                      :weight 6}]
                 :B [{:vertex :A
                      :weight 5}]
                 :Z []}]

      (is (= 1 (sut/eccentricity graph :A))))

    (let [graph {:A [{:vertex :B
                      :weight 6}]
                 :B [{:vertex :C
                      :weight 5}]
                 :C [{:vertex :D
                      :weight 5}]
                 :D []}]

      (is (= 3 (sut/eccentricity graph :A))))

    (let [graph {:A [{:vertex :B
                      :weight 6}]
                 :B [{:vertex :C
                      :weight 4}
                     {:vertex :D
                      :weight 3}
                     {:vertex :E
                      :weight 2}
                     {:vertex :F
                      :weight 1}
                     {:vertex :G
                      :weight 5}]
                 :C [{:vertex :D
                      :weight 5}]
                 :D []
                 :E []
                 :F []
                 :G []}]

      (is (= 1 (sut/eccentricity graph :B))))

    (let [graph {:A [{:vertex :B
                      :weight 6}]
                 :B [{:vertex :C
                      :weight 4}
                     {:vertex :H
                      :weight 4}]

                 :C [{:vertex :D
                      :weight 5}]
                 :D [{:vertex :E
                      :weight 5}]
                 :E [{:vertex :F
                      :weight 5}]
                 :F [{:vertex :G
                      :weight 5}]
                 :G [{:vertex :Y
                      :weight 5}]

                 :H [{:vertex :I
                      :weight 5}]
                 :I [{:vertex :J
                      :weight 5}]
                 :J [{:vertex :K
                      :weight 5}]
                 :K [{:vertex :L
                      :weight 5}]
                 :L [{:vertex :M
                      :weight 5}]
                 :M [{:vertex :Z
                      :weight 5}]

                 :Y []
                 :Z []}]

      (is (= 7 (sut/eccentricity graph :B)))))
  )

(deftest radius-test
  (testing "Radius"
    (let [graph {:E [{:vertex :D, :weight 8} {:vertex :A, :weight 59} {:vertex :F, :weight 41} {:vertex :B, :weight 72}],
                 :C [{:vertex :E, :weight 17} {:vertex :A, :weight 71} {:vertex :F, :weight 57}],
                 :B [{:vertex :E, :weight 1}
                     {:vertex :A, :weight 59}
                     {:vertex :F, :weight 79}
                     {:vertex :D, :weight 77}
                     {:vertex :C, :weight 56}],
                 :D [{:vertex :A, :weight 91}
                     {:vertex :F, :weight 30}
                     {:vertex :B, :weight 41}
                     {:vertex :C, :weight 77}
                     {:vertex :E, :weight 84}],
                 :A [{:vertex :F, :weight 26}
                     {:vertex :D, :weight 47}
                     {:vertex :B, :weight 44}
                     {:vertex :C, :weight 54}
                     {:vertex :E, :weight 93}],
                 :F []}]
      (is (= 0 (sut/radius graph))))

    (let [graph {:A [{:vertex :E, :weight 0} {:vertex :B, :weight 62} {:vertex :D, :weight 42} {:vertex :C, :weight 78}],
                 :D [{:vertex :A, :weight 3} {:vertex :B, :weight 59} {:vertex :C, :weight 44} {:vertex :E, :weight 6}],
                 :E [{:vertex :C, :weight 22} {:vertex :A, :weight 55} {:vertex :D, :weight 4} {:vertex :B, :weight 67}],
                 :B [{:vertex :A, :weight 41} {:vertex :D, :weight 83} {:vertex :C, :weight 77} {:vertex :E, :weight 98}],
                 :C [{:vertex :A, :weight 20} {:vertex :D, :weight 86} {:vertex :B, :weight 27} {:vertex :E, :weight 8}]}]
      (is (= 2 (sut/radius graph))))))


(deftest diameter-test
  (testing "Diameter"
    (let [graph {:E [{:vertex :D :weight 8} {:vertex :A, :weight 59} {:vertex :F, :weight 41} {:vertex :B, :weight 72}],
                 :C [{:vertex :E :weight 17} {:vertex :A, :weight 71} {:vertex :F, :weight 57}],
                 :B [{:vertex :E :weight 1}
                     {:vertex :A :weight 59}
                     {:vertex :F :weight 79}
                     {:vertex :D :weight 77}
                     {:vertex :C :weight 56}],
                 :D [{:vertex :A :weight 91}
                     {:vertex :F :weight 30}
                     {:vertex :B :weight 41}
                     {:vertex :C :weight 77}
                     {:vertex :E :weight 84}],
                 :A [{:vertex :F :weight 26}
                     {:vertex :D :weight 47}
                     {:vertex :B :weight 44}
                     {:vertex :C :weight 54}
                     {:vertex :E :weight 93}],
                 :F []}]
      (is (= 3 (sut/diameter graph))))

    (let [graph {:A [{:vertex :E, :weight 0} {:vertex :B, :weight 62} {:vertex :D, :weight 42} {:vertex :C, :weight 78}],
                 :D [{:vertex :A, :weight 3} {:vertex :B, :weight 59} {:vertex :C, :weight 44} {:vertex :E, :weight 6}],
                 :E [{:vertex :C, :weight 22} {:vertex :A, :weight 55} {:vertex :D, :weight 4} {:vertex :B, :weight 67}],
                 :B [{:vertex :A, :weight 41} {:vertex :D, :weight 83} {:vertex :C, :weight 77} {:vertex :E, :weight 98}],
                 :C [{:vertex :A, :weight 20} {:vertex :D, :weight 86} {:vertex :B, :weight 27} {:vertex :E, :weight 8}]}]
      (is (= 4 (sut/diameter graph))))))

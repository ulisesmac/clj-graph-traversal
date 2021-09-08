(ns graph.impl)

;; Namespace designated to contain aux functions

(def max-rand-weight 1000)

(def alphabet-length
  (- (int \Z) (int \A)))

(def num->letter
  (zipmap (range 0 alphabet-length)
          (map (comp str char) (range (int \A) (inc (int \Z))))))


(defn keyword-vertices-seq
  "Takes `length` and returns a keywords' sequence of that length
  with the format:
  :A, :B, :C, ..., :Z, :A1, :B1, :C1, ... :Z1, :A2, B2 ...
  (This helps to have clearer vertices' name)"
  [length]
  (map #(let [suffix (quot % alphabet-length)]
          (cond-> (num->letter (mod % alphabet-length))
            (not (zero? suffix)) (str suffix)
            :always               keyword))
       (range length)))


(defn randomly-divide
  "Creates a vector of `num-spaces` vectors with `elements`
   randomly distributed."
  ([elements num-spaces]
   (randomly-divide (vec elements) num-spaces []))
  ([remaining-elements num-spaces spaces-filled]
   (if (<= num-spaces 1)
     (vec (conj spaces-filled remaining-elements))
     (let [num-elems         (rand-int (inc (count remaining-elements)))
           elements-selected (vec (take num-elems remaining-elements))]
       (recur (subvec remaining-elements num-elems)
              (dec num-spaces)
              (conj spaces-filled elements-selected))))))


(defn- create-directed-adjacencies
  "takes a `vertex`, a `graph` and a sequence of adjacencies and
  returns a vector with the format for directed graphs that represents
  the adjacencies already existent in `graph` to and from `vertex`
  added to the new `adjs`."
  [vertex adjs graph]
  (vec (mapcat (fn [new-adj]
                 (if (< (rand) 0.5)
                   [[vertex {:vertex new-adj
                             :weight (inc (rand-int max-rand-weight))}]
                    [new-adj (get new-adj graph [])]]
                   [[new-adj {:vertex vertex
                              :weight (inc (rand-int max-rand-weight))}]]))
               adjs)))


(defn create-random-graph-connected
  "Creates a random simple connected graph with `num-vertices` vertices
  and (num-vertices - 1) edges."
  ([num-vertices]
   (let [vertices (vec (shuffle (keyword-vertices-seq num-vertices)))]
     (create-random-graph-connected vertices {})))
  ([[v & tail-v :as vertices] graph]
   (cond
     (empty? vertices) graph

     (empty? graph)    (recur tail-v {v []})

     :else
     (let [num-vertices-to-add  (inc (rand-int (count vertices)))
           vertices-in-graph    (count graph)
           vertices-to-add      (take num-vertices-to-add vertices)
           vertices-partitioned (randomly-divide vertices-to-add vertices-in-graph)
           ;;
           vertices-remaining   (subvec (vec vertices) num-vertices-to-add)
           new-graph            (reduce (fn [new-graph [[vertex _] new-adjs]]
                                          (reduce (fn [acc [v-name adjs]]
                                                    (update acc v-name #(if (empty? adjs)
                                                                         []
                                                                         (vec (conj % adjs)))))
                                                  new-graph
                                                  (create-directed-adjacencies vertex new-adjs new-graph)))
                                        graph
                                        (partition 2 (interleave graph vertices-partitioned)))]

       (recur vertices-remaining new-graph)))))


(defn bounded-memoize [function limit] (let [map (ref {}) vector (ref [])]
  ; Arguments of passed function.
  (fn [& args]
    ; Result search by arguments. If map returns nil - sequence is jumping to an actual calculations.
    (if-let [cachedResult (find @map args)]
      ; If map returns result - (val cachedResult), end.
      (val cachedResult)
      ; Actual calculations.
      (let [calculatedResult (apply function args)]
        (dosync
          ; Is cache full?
          (when (= (count @vector) limit)
            ; If it is, making room for calculatedResult.
            (alter map dissoc (first @vector))
            (alter vector subvec 1))
          ; Adding calculatedResult to cache
          (alter map assoc args calculatedResult)
          (alter vector conj args))
        ; return calculatedResult.
        ;(println "calculating")
        calculatedResult)))))

(def ** (bounded-memoize * 3))

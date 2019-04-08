(ns holidays.util)

(defn map-keys
  "apply f to each key in map m"
  [f m]
  (zipmap (map f (keys m)) (vals m)))

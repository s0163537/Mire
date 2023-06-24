(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *money*)
(def ^:dynamic *loot*)
(def ^:dynamic *arrows* (ThreadLocal.))

(def damage 25)
(def prompt "> ")
(def streams (ref {}))
(def health (ref {}))
(def score (ref {}))
(def lives (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))

(defn change-points [points]
  "Добавить очки текущему игроку"
  (dosync
    (commute score assoc *name* (+ (@score *name*) points))))

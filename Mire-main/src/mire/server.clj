(ns mire.server
  (:require [clojure.java.io :as io]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]))

(defn- cleanup []
  "Сбросьте весь инвентарь и удалите игрока из комнаты и списка игроков."
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*)
            disj player/*name*)))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "Это имя используется; Повторить: ")
        (flush)
        (recur (read-line)))
    name))


(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    ;; We have to nest this in another binding call instead of using
    ;; the one above so *in* and *out* will be bound to the socket
    (print "\nКак Вас зовут? ") (flush)
    (def Money 0)
    (binding [player/*name* (get-unique-player-name (read-line))
              player/*current-room* (ref (@rooms/rooms :fsittingroom))
              player/*inventory* (ref #{})
              player/*money* (ref Money)
              ]
      (dosync
       (commute (:inhabitants @player/*current-room*) conj player/*name*)
       (commute player/streams assoc player/*name* *out*)
       (commute player/health assoc player/*name* 100)
       (commute player/score assoc player/*name* 0)
       (commute player/lives assoc player/*name* "live"))
       (.set player/*arrows* 0)
      (println (commands/look)) (print player/prompt) (flush)

      (try (loop [input (read-line)]
            (if (<(int (@player/health player/*name*)) 1)

            (when true
              (cleanup)
              (println (commands/execute "deadplayer"))
              (.flush *err*)
              (print player/prompt) (flush)
              (recur (read-line)))

            (when input
              (println (commands/execute input))
              (.flush *err*)
              (print player/prompt) (flush)
              (recur (read-line)))))
           (finally (cleanup))))))

(defn -main
  ([port dir]
     (rooms/add-rooms dir)
     (defonce server (socket/create-server (Integer. port) mire-handle-client))
     (println "Launching Mire server on port" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))

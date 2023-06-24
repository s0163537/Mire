(ns mire.commands
  (:use [mire.rooms :only [rooms room-contains? room-contains-gold? room-contains-loot?]]
        [mire.player :as player])
  (:use [clojure.string :only [join]]))

(defn- move-between-refs
  "Переместите один экземпляр obj между от и до. Необходимо вызвать транзакцию."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn- move-delete
  [obj from]
  (alter from disj obj))

;; Command functions

(defn look
  "Получите описание окружающих окрестностей и их содержимого."
  []
  (str (:desc @*current-room*)
       "\r\nВыходы: " (keys @(:exits @*current-room*)) "\r\n"
       (join "\r\n" (map #(str "Там " % " находится здесь.\r\n")
                           @(:items @*current-room*)))
       (join "\r\n" (map #(str "Игрок " % " находится здесь.\r\n")
                           @(:inhabitants @*current-room*)))
       (join "\r\n" (map #(str "Там " % " находится здесь.\r\n")
                           @(:loot @*current-room*)))
       (join (str "Золото " @(:gold @*current-room*) " здесь.\r\n"))
       (join (str "Здоровье: " (@health *name*) ".\r\n"))
       (join (str "Очки: " (@score *name*) ".\r\n"))
       (join (str "Жизнь: " (@lives *name*) ".\r\n"))
  ))

(defn players
  "Получить список игроков"
  []
  (str
      (doseq [inhabitant (keys @lives)]
         (println (str inhabitant ":" (@lives inhabitant)))
    )

  ))
(defn move
  "\"♬ Мы должны выбраться отсюда... ♪\" Дайте направление."
  [direction]
  (dosync
   (let [target-name ((:exits @*current-room*) (keyword direction))
         target (@rooms target-name)]
     (if (not= @( :lock target) #{(some @( :lock target) @*inventory*)})
        (if (not= @( :lock target) #{})
           ( str "ЗАКРЫТО! Найдите " @( :lock target) " Сдать " )
        (if target
           (do
             (move-between-refs *name*
                                (:inhabitants @*current-room*)
                                (:inhabitants target))
             (ref-set *current-room* target)
             (look))
        "Вы не можете пойти по этому пути."))
    (if target
       (do
         (move-between-refs *name*
                            (:inhabitants @*current-room*)
                            (:inhabitants target))
         (ref-set *current-room* target)
         (look))
    "Вы не можете пойти по этому пути.")))))

(defn grab
  "Поднимите что-нибудь."
  [thing]
  (dosync
    (cond
    (or (= thing "coin") (= thing "treasuregold") (= thing "bagmoney"))
      (if (room-contains-gold? @*current-room* thing)
        (do
          (case thing
            "монета"
            (do (alter *money* inc) (change-points 1))
            "рюкзак денег"
            (do (alter *money* + 7) (change-points 7))
            "сокровищница золота"
            (do (alter *money* + 15) (change-points 15))
          )
          (if (= ((keyword thing) @(:gold @*current-room*)) 1)
            (alter (:gold @*current-room*) dissoc (keyword thing))
            (do
              (def temp-gold ((keyword thing) @(:gold @*current-room*)))
              (alter (:gold @*current-room*) dissoc (keyword thing))
              (alter (:gold @*current-room*) assoc (keyword thing) (- temp-gold 1))
            )
          )
          (str " Вы подобрали " thing ".")
        )
        (str " Нет никаких " thing " здесь.")
      )

      (room-contains? @*current-room* thing)
        (case thing
          "стрелы" (do
            (.set player/*arrows* (+ (.get player/*arrows*) 5))
            (move-delete (keyword thing) (:items @*current-room*))
            (println "Вы подобрали стрелки.")
            )
            (do
              (move-between-refs (keyword thing)
                                 (:items @*current-room*)
                                 *inventory*)
              (str "Вы подобрали " thing ".")
            )
        )
      :default (str "Нет никаких " thing " здесь.")
      )
    )
  )

(defn seemoney
  "Смотрите свои деньги"
  []
  (str (join "\r\n" (map #(str "Деньги – это " % " .\r\n") [(str @*money*)])))
)

(defn discard
  "Положите что-нибудь, что вы несете."
  [thing]
  (if (= #{(keyword thing)} @( :lock @*current-room*))
   (str "Тут не закинешь " @( :lock @*current-room*))
  (dosync
   (if (or (= thing "coin") (= thing "treasuregold") (= thing "bagmoney"))
        (case thing
          "монета" (if (> @*money* 0)
                    (do
                      (alter *money* dec)
                      (change-points -1)
                      (if (room-contains-gold? @*current-room* thing)
                        (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                        (def temp-gold 0)
                      )
                      (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                      (str "Вы уронили " (keyword thing) ".")
                    )
                    (str "Денег не хватает!")
                  )
          "рюкзак денег" (if (>= @*money* 7)
                        (do
                          (alter *money* - 7)
                          (change-points -7)
                          (if (room-contains-gold? @*current-room* thing)
                            (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                            (def temp-gold 0)
                          )
                          (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                          (str "Вы уронили " (keyword thing) ".")
                        )
                        (str "Денег не хватает!")
                      )
          "сокровищница золота" (if (>= @*money* 15)
                        (do
                          (alter *money* - 15)
                          (change-points -15)
                          (if (room-contains-gold? @*current-room* thing)
                            (def temp-gold ((keyword thing) @(:gold @*current-room*)))
                            (def temp-gold 0)
                          )
                          (alter (:gold @*current-room*) assoc (keyword thing) (+ temp-gold 1))
                          (str "Вы уронили " (keyword thing) ".")
                        )
                        (str "Денег не хватает!")
                      )
        )
        (if (carrying? thing)
          (do (move-between-refs (keyword thing)
                                 *inventory*
                                 (:items @*current-room*))
              (str "Вы уронили " thing ".")
          )
          (str "Вы не несете " thing ".")
        )
      )
    )
  )
)

(defn inventory
  "Посмотрите, что у вас есть."
  []
  (str "Вы несете:\r\n"
       (join "\r\n" (seq @*inventory*))
       "\nУ вас есть " (.get player/*arrows*) " стрелы."
  )
)

(defn detect
  "Если у вас есть детектор, вы можете видеть, в какой комнате находится предмет."
  [item]
  (if (@*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms)))]
      (str item " находится в " (:name room))
      (str item " не находится ни в одной комнате."))
    "Для этого вам нужно иметь при себе детектор."))

(defn say
  "Произнесите что-нибудь вслух, чтобы все в комнате могли услышать."
  [& words]
  (let [message (join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @*current-room*) *name*)]
      (binding [*out* (streams inhabitant)]
        (println *name* " : " message)
        (println prompt)))
    (str "Вы сказали " message)))

(defn help
  "Показать доступные команды и то, что они делают."
  []
  (join "\r\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

(defn attack
  "Атаковать другого игрока"
  [target]
  (dosync
    (if (contains? @health target)
      (if (contains? @(:inhabitants @*current-room*) target)
        (do
          (if (not= (@lives target) "смерть")

            (do
          (commute health assoc target (- (@health target) damage))
          (if (< (int(@health target)) 1)
           ((commute lives assoc target "смерть")
           (println
          (say (str target " убит " *name* "\r\n")))
          (commute score assoc *name* (+ (@score *name*) 25)))
          )

          "Успешная атака.")
          "Он мертв")
        )
        "Такой цели в комнате нет."
      )
      "Target не существует."
    )
  )
)

(defn shoot
  "Shoot another player"
  [target]
  (dosync
    (if (player/carrying? :bow)
      (if (> (.get player/*arrows*) 0)
        (if (contains? @health target)
          (if (contains? @(:inhabitants @*current-room*) target)
            (do
              (commute health assoc target (- (@health target) 50))
              (.set player/*arrows* (- (.get player/*arrows*) 1))
              "Great shot!"
            )
            "No such target in the room."
          )
          "Target doesn't exist."
        )
        "You don't have arrows."
      )
      "You don't have a bow."
    )
  )
)

(defn buy
		"Buy loot from any place"
		[loot]
		(dosync
    (if (or (= loot "sword"))
        			(do
			          (case loot
			            "sword"
			            (if (> @*money* 7)
			            		(do
			            		(move-between-refs (keyword loot)
			                             		(:items @*current-room*)
			                             		*inventory*)
			            		(alter *money* - 7)
			           			(str "You bought the " loot ".")
			           			)
			      						)
      							)
											)
					(str "There is no " loot " in the shop.")
			)
	)
)

;; Command data
(defn deadplayer
  []
  (str "You are dead \r\n"
  "You score:" (@score *name*) "\r\n"
  ))

(def commands
              {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "seemoney" seemoney
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "players" players
               "help" help
               "attack" attack
               "buy" buy
               "deadplayer" deadplayer
               "shoot" shoot
               })

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))

(ns mire.challenges
  (:require [mire.player :as player]))

(def challenges (ref {})) ; Maps challenge IDs to challenge data refs

(defn create-challenge [creator challenge-type challenge-name]
  (dosync
   (let [new-challenge (ref {:name challenge-name
                             :type challenge-type
                             :creator creator
                             :participants #{creator}
                             :status :waiting
                             :scores {}})]
     (alter challenges assoc challenge-name new-challenge)
     true)))

(defn join-challenge [challenge-name player]
  (dosync
   (if-let [challenge-ref (get @challenges challenge-name)]
     (let [challenge @challenge-ref]
       (if (= (:status challenge) :waiting)
         (do
           (alter challenge-ref update :participants conj player)
           true)
         false))
     false)))

(defn start-challenge [challenge-name creator]
  (dosync
   (if-let [challenge-ref (get @challenges challenge-name)]
     (let [challenge @challenge-ref]
       (if (and (= (:status challenge) :waiting)
                (= (:creator challenge) creator))
         (do
           (alter challenge-ref assoc :status :in-progress)
           true)
         false))
     false)))

(defn find-active-challenge [player]
  (some (fn [[_ challenge-ref]]
          (let [challenge @challenge-ref]
            (when (and (= (:status challenge) :in-progress)
                       (contains? (:participants challenge) player))
              challenge-ref)))
        @challenges))

(defn add-score [challenge-ref player points]
  (dosync
   (alter challenge-ref update-in [:scores player] (fnil + 0) points)))
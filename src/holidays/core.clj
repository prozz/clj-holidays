(ns holidays.core
  (:use [holidays.util])
  (:require [clj-time.core :as t])
  (:require [clj-time.periodic :as p])
  (:require [clj-time.format :as f])
  (:require [clj-time.predicates :as pr]))

(def date-format
  (f/formatter "yyyy-MM-dd"))

(defn str->date
  "convert string in date-format to date obj"
  [str]
  (f/parse date-format str))

(def date str->date)

(defn date->str
  "convert date obj to string in date-format"
  [date]
  (f/unparse date-format date))

(def bank-holidays
  (map-keys str->date {"2018-01-01" "Nowy Rok"
                       "2018-01-06" "Święto Trzech Króli"
                       "2018-04-01" "Wielkanoc"
                       "2018-04-02" "Poniedziałek Wielkanocny"
                       "2018-05-01" "Święto Pracy"
                       "2018-05-03" "Święto Konstytucji 3 Maja"
                       "2018-05-20" "Zielone Świątki"
                       "2018-05-31" "Boże Ciało"
                       "2018-08-15" "Wniebowzięcie Najświętszej Maryi Panny"
                       "2018-11-01" "Wszystkich Świętych"
                       "2018-11-11" "Święto Niepodległości"
                       "2018-12-25" "Pierwszy dzień Bożego Narodzenia"
                       "2018-12-26" "Drugi dzień Bożego Narodzenia"}))

(defn bank-holiday?
  "defined manualy as an entry to bank-holidays, can be replaced in future
  by something similar to https://github.com/polygloton/calendar-logic/"
  [date]
  (contains? bank-holidays date))

(defn holiday?
  "any day that is weekend or bank holiday"
  [date]
  (or (pr/weekend? date)
      (bank-holiday? date)))

(def workday?
  "any day that is not a holiday"
  (complement holiday?))

(defn calendar
  "infinite lazy sequence of dates starting from start-date, the calendar"
  [start-date]
  (p/periodic-seq start-date (t/days 1)))

(defn holidays
  "infinite lazy sequence of holiday dates starting from start-date, the holidays calendar"
  [start-date]
  (filter holiday? (calendar start-date)))

(defn weekends
  "infinite lazy sequence of weekend dates starting from start-date, the weekends calendar"
  [start-date]
  (filter pr/weekend? (calendar start-date)))

(defn workdays
  "infinite lazy sequence of working days dates starting from start-date, the workdays calendar"
  [start-date]
  (filter workday? (calendar start-date)))

(defn as-days
  "list of individual days from given interval filtered by calendar entries,
   i.e.: workdays, holidays, weekends or just calendar"
  [interval calendar]
  (take-while #(t/within? interval %1) calendar))

(defn overlaps?
  "checks if at least one of the intervals overlap with given interval"
  [intervals interval]
  (some (partial t/overlaps? interval) intervals))

(defn leave-request
  "creates leave-request map"
  [start-date total-days type]
  (let [; end date is inclusive
        end-date (last (take total-days (workdays start-date)))
        ; intervals are exclusive as per clj-time docs, hence add 1 day
        interval (t/interval start-date (t/plus end-date (t/days 1)))]
    {:type type
     :start-date start-date
     :end-date end-date
     :interval interval
     :total-days total-days}))

(defn add
  ([leave-request]
   (add '() leave-request))
  ([requests leave-request]
   (if-not
    (or (holiday? (:start-date leave-request)) ; cannot start on weekend or holiday
        (overlaps? (map :interval requests) (:interval leave-request))) ; cannot overlap with previous leave requests
     (conj requests leave-request))))

(defn total-days-off
  "calculate total days off from given requests"
  [requests]
  (reduce + (map :total-days requests)))

(defn of-type
  "only requests of given type"
  [type requests]
  (filter #(= type (:type %)) requests))

(def sick-leaves (partial of-type :sick-leave))

(defn extract-requests
  "take a list of holidays maps and return a list of all leave-requests"
  [holidays]
  (reduce #(concat %1 (:requests %2)) '() holidays))

(def john-holidays
  {:name "john"
   :requests (some-> (add (leave-request (date "2019-01-04") 5 :annual))
                     (add (leave-request (date "2019-01-24") 1 :annual))
                     (add (leave-request (date "2019-06-04") 5 :sick-leave))
                     (add (leave-request (date "2019-10-04") 3 :sick-leave)))})

(def mary-holidays
  {:name "mary"
   :requests (some-> (add (leave-request (date "2019-01-03") 7 :annual))
                     (add (leave-request (date "2019-02-28") 30 :maternity))
                     (add (leave-request (date "2019-11-07") 14 :sick-leave)))})

mary-holidays
john-holidays

(-> john-holidays
    :requests
    sick-leaves
    total-days-off)

(-> [john-holidays  mary-holidays]
    extract-requests
    sick-leaves
    total-days-off)

;---- experiments ahead

(defn leave-request-fragment
  [name leave-request]
  (let [start-date (:start-date leave-request)
        interval (:interval leave-request)
        days (as-days interval (workdays start-date))
        type (:type leave-request)]
    (map #(assoc {} :name name :type type :date %) days)))

(defn holidays-fragment
  [holidays]
  (apply concat (map (partial leave-request-fragment (:name holidays)) (:requests holidays))))

(holidays-fragment john-holidays)
(leave-request-fragment "john" (-> john-holidays :requests first))

(->> (map holidays-fragment [john-holidays mary-holidays])
     (apply concat)
     (group-by :date))



(ns date-utils.core-test
  (:require [clj-time.core   :as tc]
            [clj-time.format :as tf]
            [clj-time.coerce :as te]
            [clojure.string :as str]
            [clojure.test :refer :all])
  (:import org.joda.time.format.DateTimeFormatter
           org.joda.time.DateTime
           org.joda.time.DateTimeZone
           java.util.List
           java.util.regex.Pattern))

;; ************************************************

;; WHY don't transform value date or time to only numbers and then try to parse ... cause we need to throw error related to user format exceptions

;; ************************************************

;; The following times all refer to the same moment: "18:30Z", "22:30+04", "1130−0700", and "15:00−03:30".
;; Nautical time zone letters are not used with the exception of Z.

;;http://stackoverflow.com/questions/16886342/how-can-i-unparse-a-date-in-a-specific-time-zone

(tf/unparse (tf/with-zone (:date-time-no-ms tf/formatters)
              ;;"Africa/Abidjan"
              (tc/time-zone-for-id (first (DateTimeZone/getAvailableIDs))))
            (tc/now))

(tf/unparse (tf/with-zone (:date-time-no-ms tf/formatters)
              (tc/time-zone-for-id "America/Chicago"))
            (tc/now))

(tf/unparse (tf/with-zone (:date-time-no-ms tf/formatters)

              (tc/time-zone-for-offset -5 0))
            (tc/now))

(deftest define-new-api
  ".... defining new api for querying messages by date"

  (testing "....more..."

    (is (= 1 1))
    )
  )


(def ^DateTimeFormatter default-formatter
  ;; yyyy-MM-dd
  (:date tf/formatters))

;;http://joda-time.sourceforge.net/apidocs/org/joda/time/format/ISODateTimeFormat.html
;;http://en.wikipedia.org/wiki/List_of_UTC_time_offsets
;;http://stackoverflow.com/questions/15967642/how-to-list-the-timezone-offset-timezone-id-and-long-name-in-joda-time

(tf/parse (:basic-date tf/formatters) "20141201")
(tf/parse (:date tf/formatters) "2014-12-02")
(tf/unparse (:basic-date tf/formatters)
            (tf/parse (:basic-date tf/formatters) "20141202"))

;; Z sufix UK HOUR +00:00
(tf/parse (:basic-date-time-no-ms tf/formatters) "20141202T000000Z")

;; UKRAINE +02:00
(tf/parse (:basic-date-time-no-ms tf/formatters) "20141202T000000+0200")
;; EST TIME -05:00
(tf/parse (:basic-date-time-no-ms tf/formatters) "20141202T000000-0500")
;; French Polynesia -10:00
(tf/parse (:basic-date-time-no-ms tf/formatters) "20141202T000000-1000")

(tf/unparse (:basic-date-time-no-ms tf/formatters)
            (tf/parse (:basic-date-time-no-ms tf/formatters) "20141202T000000Z"))

;; only works Z
(tf/parse (:date-time-no-ms tf/formatters) "2014-12-02T00:05:00Z")


(defn substring? [^String sub ^String st]
  (not= (.indexOf st sub) -1))

(defn has-colons? [^String s]
  (substring? ":" s ))

(def conditions
  {:numbers {:regex #"\d+"
             :ex-fn-message #(str "You need to use only numbers")}
   :numbers+hyphens {:regex #"[\d-]+"
                     :ex-fn-message #(str "in date format only permited numbers and hiphens")}
   :numbers+colons {:regex #"[\d:]+"
                    :ex-fn-message #(str "in time format only permited numbers and colons")}
   :numbers+colons+Z {:regex #"[Z\d:+-]+"
                      :ex-fn-message #(str "in time-zone format only permited letter 'Z', numbers and colons")}
   :numbers+YMDH {:regex #"[\d[YMDH]]+"
                  :ex-fn-message #(format "Duration pattern only can contain unsigned numbers and these letters 'YMDH' as this pattern nYnMnDnH, you provided %s" %)}

   })

(defn satisfy-pattern? [^Pattern p]
  (fn [^String s] (= s (re-find p s))))

(defn check-condition [value condition-fn exception-message-fn]
  (when-not (condition-fn value)
    (throw (Exception. (exception-message-fn value)))))

(defn check-pattern-condition
  ([key-condition value]
     (check-pattern-condition key-condition value (:ex-fn-message (get conditions key-condition)))
     (let [{:keys [regex ex-fn-message]} (get conditions key-condition)]
       (check-condition value (satisfy-pattern? regex) ex-fn-message)))
  ([key-condition value ex-fn-message]
     (let [{:keys [regex]} (get conditions key-condition)]
       (check-condition value (satisfy-pattern? regex) ex-fn-message)))
  )


(defn validate-all-numbers [^String s]
  (= s (re-find #"\d+" s)))


(defn validate-time-no-colons [^String s]
 (and (contains? #{2 4 6} (count s)) (validate-all-numbers s)))

;; (defn validate-dur [^String s]
;;   (= s (re-find #"[\dYMDH]" s))
;;   )

(defn parse-int [^String s]
  (. Integer parseInt  s)
  )

(defn valid-year [^String s]
  (let [y (parse-int s)]
    (and (> y 2000) (<= y (inc (tc/year (tc/now)))))))
(defn valid-month [^String s]
  (let [m (parse-int s)]
    (and (> m 0 ) (<= m 12))))
(defn valid-day [^String s]
  (let [d (parse-int s)]
    (and (> d 0 ) (<= d 31))))
(defn valid-hour [^String s]
  (let [m (parse-int s)]
    (and (>= m 0 ) (< m 24))))
(defn valid-minute [^String s]
  (let [m (parse-int s)]
    (and (>= m 0 ) (< m 60))))

(defn take-val [start n ^String s]
  (apply str (take n (drop start (seq s)))))

(def date-hiphens-formatter (:date tf/formatters))
(def date-no-hiphens-formatter (:basic-date tf/formatters))

;(take-val 8 2 "2012-03-12")


(defmulti parse-date (fn [s] (if (substring? "-" s )
                              :with-hiphens
                              :just-numbers)))

(defmethod parse-date :with-hiphens [s]
  ;; if only year then it's parsed as no hiphens
  ;; so we need only to check for YYYY-MM or YYYY-MM-DD
  (check-condition s #(contains? #{7 10} (count %))
                   #(format "Invalid date value. Example: 2014-12-30. You provided: %s" %))
  [date-hiphens-formatter
   (condp = (count s)
     7 (do
         ;;"YYYY-MM"
         (check-condition s (fn [_] (and (valid-year (take-val 0 4 s)) (valid-month (take-val 5 2 s))))
                          #(format "invalid year or month values, you provided : %s" %))
         (format "%s-%s-01" (take-val 0 4 s) (take-val 5 2 s)))
     10
     (do
       ;;"YYYY-MM-DD"
       (check-condition s (fn [_] (and (valid-year (take-val 0 4 s)) (valid-month (take-val 5 2 s)) (valid-day (take-val 8 2 s))))
                        #(format "invalid year or month or day values, you provided : %s" %))
        s))])

(defmethod parse-date :just-numbers [s]
  (check-pattern-condition :numbers s
                           #(format "You need to use only numeric values, example YYYYMMDD 20141230. You provided: %s" %))
  (check-condition s #(contains? #{4 6 8} (count %))
                   #(format "you have invalid format date, example YYYYMMDD 20141230. You provided: %s" %))

  [date-no-hiphens-formatter
   (condp = (count s)
     4 (do
         ;;"YYYY"
         (check-condition #(valid-year %) #(str "invalid year value"))
         (format "%s0101" s))
     6 (do
         ;;"YYYYMM"
         (check-condition s (fn [_] (and (valid-year (take-val 0 4 s)) (valid-month (take-val 4 2 s))))
                          #(format "invalid year or month values, you provided : %s" %))
         (format "%s%s01" (take-val 0 4 s) (take-val 4 2 s)))
     8 (do
         ;;"YYYYMMDD"
         (check-condition s (fn [_] (and (valid-year (take-val 0 4 s)) (valid-month (take-val 4 2 s)) (valid-day (take-val 6 2 s))))
                          #(format "invalid year or month or day values, you provided : %s" %))
         s))])

(parse-date "2012-02-32")



;; An offset of zero, in addition to having the special representation "Z", can also be stated numerically as "+00:00", "+0000", or "+00". However, it is not permitted to state it numerically with a negative sign, as "−00:00", "−0000", or "−00".
(defn take-off-timezone [^String ftime]
  (cond
   (substring? "Z" ftime) (do
                           ;;"zulu"
                          (apply str (butlast ftime))
                          )
   (substring? "+" ftime) (do
                            ;;"utc+"
                            (first (str/split ftime #"\+"))
                            )
   (substring? "-" ftime) (do
                            ;;"utc-"
                            (first (str/split ftime #"\-"))
                            )
   :else (do
           "no timezone"
           ftime)))
;;(take-off-timezone "00:05:00+08:00")

(defn get-timezone [^String ftime]
  (cond
   (substring? "Z" ftime) (do
                           ;;"zulu"
                            "Z"
                          )
   (substring? "+" ftime) (do
                            ;;"utc+"
                            (str "+"(last (str/split ftime #"\+")))
                            )
   (substring? "-" ftime) (do
                            ;;"utc-"
                            (str "-" (last (str/split ftime #"\-")))
                            )
   :else (do
           ;;"no timezone"
           "Z")))

(defn parse-time [^String s]
  (if (has-colons? s)
       (do
         ;; if only HOUR then it's parsed as no colons
         ;; so we need only to check for hour+minute and hour+minute+second
         "time with colons"
         (let [time-values (str/split s #":")]
          (condp = (count time-values)
            2 (do
                ;;"HH-MM"
                (let [[hour minute] time-values]
                  (if (and (valid-hour hour) (valid-minute minute))
                    [ (:date-time-no-ms tf/formatters) (format "%s:%s:00" hour minute)]
                    (throw (Exception. "invalid hour or minute in your time value")))))
            3 (do
                ;;"HH-MM-SS"
                (let [[hour minute second] time-values]
                  (if (and (valid-hour hour) (valid-minute minute) (valid-minute second))
                    [(:date-time-no-ms tf/formatters) s]
                    (throw (Exception. "invalid hour or minute or second in your time value")))))

            ;; you are giving something as 12:12:12:12
            (throw (Exception. "invalid time value. Example: 12:59:15")))))
       (if (validate-time-no-colons s)
         (condp = (count s)
           2 (do
               ;;"HH"
               (if (valid-hour s)
                 [(:basic-date-time-no-ms tf/formatters) (format "%s0000" s)]
                 (throw (Exception. "invalid hour value"))
                 )
               )
           4 (do
               ;;"HHMM"
               (let [hour (take-val 0 2 s)
                     minute (take-val 2 2 s)]
                 (if (and (valid-hour hour) (valid-minute minute))
                 [(:basic-date-time-no-ms tf/formatters) (format "%s%s00" hour minute)]
                   (throw (Exception. "invalid hour or minute value"))
                   )))
           6 (do
               ;;"HHMMSS"
               (let [hour (take-val 0 2 s)
                     minute (take-val 2 2 s)
                     second (take-val 4 2 s)]
                 (if (and (valid-hour hour) (valid-minute minute) (valid-minute second))
                 [(:basic-date-time-no-ms tf/formatters) (format "%s%s%s" hour minute second)]
                   (throw (Exception. "invalid hour or minute or second value"))
                   )))



             )
         (throw (Exception. "you have invalid format time, example HHMM 2345"))))
  )

(defn validate-timezone-no-colons [ftime]
  (let [timezone- (get-timezone ftime)]
    (if (not= timezone- "Z")
      (let [s (apply str (next timezone-)) ;; take of +/-
            ]
        (if (validate-time-no-colons s)
         (condp = (count s)
           2 (do
               ;;"HH"
;               (check)
               (if (valid-hour s)
                 s
                 (throw (Exception. "invalid hour value in your time zone"))
                 )
               )
           4 (do
               ;;"HHMM"
               (let [hour (take-val 0 2 s)
                     minute (take-val 2 2 s)]
                 (if (and (valid-hour hour) (valid-minute minute))
                   s
                   (throw (Exception. "invalid hour or minute value in your time-zone"))
                   )))
           6 (do
               ;;"HHMMSS"
               (let [hour (take-val 0 2 s)
                     minute (take-val 2 2 s)
                     second (take-val 4 2 s)]
                 (if (and (valid-hour hour) (valid-minute minute) (valid-minute second))
                   s
                   (throw (Exception. "invalid hour or minute or second value in your timezone"))))))

         (throw (Exception. "you have invalid format time in your time-zone, example HHMMSS 234500"))
         ))
      timezone-)))

(defn validate-expected-format-date-and-time [t date-formatter]
  (if-not (has-colons? t)
    (when (not= date-formatter date-no-hiphens-formatter)
      (throw (Exception. "date and time format don't be the same ")))
    (when (not= date-formatter date-hiphens-formatter)
      (throw (Exception. "date and time format don't be the same ")))))



(defn parse-date-time [^String s]
  (let [[date time] (str/split s #"T")
            time-nozone (take-off-timezone time)
            time-zone (get-timezone time)]

        (check-pattern-condition :numbers+hyphens date)

        (check-pattern-condition :numbers+colons time-nozone)
        (check-pattern-condition :numbers+colons+Z time-zone)

        (let [[date-formatter date-value] (parse-date date)
              [time-formater time-value] (parse-time time-nozone)]

          (validate-expected-format-date-and-time time date-formatter)

          (validate-timezone-no-colons time)

          (tf/parse time-formater (format "%sT%s%s" date-value time-value time-zone))
          )))

(defn parse-date-only [^String s]
  (check-pattern-condition :numbers+hyphens s)
  (apply tf/parse (parse-date s)))

(defn parse-date* [^String -s ]
  (let [s (str/upper-case -s)]
    (if (substring? "T" s )
      (parse-date-time s)
      (parse-date-only s))))

(parse-date* "2014-11-01T05:35:00+05")


;; nYnMnDnH ::::::::::::: 1Y5M4D3H :::: 1 year 5 months 4 days and 3 hours
;; nMnDnH :::::::::::::::::  5M4D3H :::::::: 5 months 4 days and 3 hours
;; nDnH ::::::::::::::::::::::  4D3H ::::::::::::: 4 days and 3 hours
;; nH :::::::::::::::::::::::::::  3H ::::::::::::::::: 3 hours
;; nY :::::::::::::::::::::::::::  3Y ::::::::::::::::: 3 years
;; nM :::::::::::::::::::::::::::  3M :::::::::::::::: 3 months
;; nD :::::::::::::::::::::::::::  3D  :::::::::::::::: 3 days

(defn parse-dur
  "Adapting String dur to this standar http://en.wikipedia.org/wiki/ISO_8601#Durations
   There is a few of String pattern validations that ensure ISO_8601

   Result is a map with 'date-fn key' and 'integer value' as follows
   (parse-dur '1Y5M4D3H')
  => {#<core$hours clj_time.core$hours@6802bf86> 3,
      #<core$days clj_time.core$days@11ad1594> 4,
      #<core$months clj_time.core$months@26e0108a> 5,
      #<core$years clj_time.core$years@5527e87d> 1}"
  [^String dur]
  (do
    (check-pattern-condition :numbers+YMDH dur)

    (check-condition dur #(even? (count %))
                     #(format "Duration pattern must be even following this pattern nYnMnDnH, you provided %s " %))

    (check-condition dur #(substring? (apply str (mapv (comp  str second) (partition 2 %))) "YMDH")
                     #(format "Duration pattern is an orderer pattern nYnMnDnH, you provided %s" %))

    (let [parsed-dur (map
                      (fn [[n t]] [(parse-int (str  n))
                                  (get {:Y tc/years :M tc/months :D tc/days :H tc/hours}
                                       (keyword (str/upper-case (str t))))])
                      (partition 2 dur))
          format-dur (reduce (fn [i [n t]] (assoc i t n)) {} parsed-dur)]

      ;; TODO improve style??
     (when-not (= (count format-dur) (count parsed-dur))
       (throw (Exception. (format "Duration value can't contain duplicated keys following this pattern nYnMnDnH, you provided %s" dur))))
     format-dur)))


(defn apply-dur-to-date [^String dur ^DateTime date]
  (let [format-dur (parse-dur dur)]
    (apply tc/plus (conj (map (fn [[f n]]
                                (f n)) format-dur) date))))

(parse-dur "1Y5M4D3H")
(apply-dur-to-date  "1Y5M4D3H" (tc/now))
(apply-dur-to-date  "0H" (tc/now))

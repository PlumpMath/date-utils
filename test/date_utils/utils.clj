(ns date-utils.utils
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





(deftest define-new-api
  ".... defining new api for querying messages by date"

  (testing "....more..."

    (is (= 1 1))
    )
  )

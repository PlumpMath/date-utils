(ns date-utils.core-test
  (:require [clj-time.core   :as tc]
            [clj-time.format :as tf]
            [clj-time.coerce :as te]
            [clojure.string :as str]
            [date-utils.core :refer :all]
            [clojure.test :refer :all])
  (:import org.joda.time.format.DateTimeFormatter
           org.joda.time.DateTime
           org.joda.time.DateTimeZone
           java.util.List
           java.util.regex.Pattern)
  )

(deftest fn-utils
  ".... defining new api for querying messages by date"

  (testing ".... simple validations..."

    (is (not  (valid-year "1")))
    (is (valid-year "2001"))
    ;; you need Strings
    (is (thrown? ClassCastException (valid-year 1)))


    (is (not  (valid-month "0")))
    (is (not  (valid-month "-1")))
    (is (valid-month "12"))
    ;; you need Strings
    (is (thrown? ClassCastException (valid-month 1)))


    (is (not  (valid-day "0")))
    (is (not  (valid-day "-1")))
    (is (not  (valid-day "32")))
    (is (valid-day "18"))
    ;; you need Strings
    (is (thrown? ClassCastException (valid-day 1)))

    (is (valid-hour "0"))
    (is (not  (valid-hour "-1")))
    (is (not  (valid-hour "24")))
    (is (valid-hour "16"))
    ;; you need Strings
    (is (thrown? ClassCastException (valid-hour 1)))


    ;; minutes and seconds use the same valid-minute
    (is (valid-minute "0"))
    (is (not  (valid-minute "-1")))
    (is (not  (valid-minute "60")))
    (is (valid-minute "50"))
    ;; you need Strings
    (is (thrown? ClassCastException (valid-minute 1))))


  (testing ".... condition validations"
    (is (nil? (check-pattern-condition :numbers "12378124387")))
    (is (thrown? Exception (check-pattern-condition :numbers "A2378124387")))


    (is (nil? (check-pattern-condition :numbers+hyphens "123-12----12")))
    (is (thrown? Exception (check-pattern-condition :numbers+hyphens "1-2-A")))


    (is (nil? (check-pattern-condition :numbers+colons "123:12::::23412343")))
    (is (thrown? Exception (check-pattern-condition :numbers+colons "1:2-2")))


    (is (nil? (check-pattern-condition :numbers+colons+Z+plus+minus "123:12::::23412343")))
    (is (nil? (check-pattern-condition :numbers+colons+Z+plus+minus "ZZZZZZZZZ")))
    (is (nil? (check-pattern-condition :numbers+colons+Z+plus+minus "ZZZZZZZZZ+++++----61827364812734")))
    (is (thrown? Exception (check-pattern-condition :numbers+colons+Z+plus+minus "1:ZAZZ-+")))


    (is (nil? (check-pattern-condition :numbers+YMDH "1Y2M3D6H")))
    (is (nil? (check-pattern-condition :numbers+YMDH "12DH")))
    (is (nil? (check-pattern-condition :numbers+YMDH "YMDH12345YMDH12345")))
    (is (nil? (check-pattern-condition :numbers+YMDH "YMDHYMDH")))
    (is (nil? (check-pattern-condition :numbers+YMDH "1234512345")))
    (is (thrown? Exception (check-pattern-condition :numbers+YMDH "1A")))
    (is (thrown? Exception (check-pattern-condition :numbers+YMDH "45F"))))



  (testing "formatting only date"
    (is (= "20120231" (format-date* "2012-02-31")))
    (is (= "20120201" (format-date* "2012-02")))
    (is (= "20120101" (format-date* "2012")))

    (is (= "20120231" (format-date* "20120231")))
    (is (= "20120201" (format-date* "201202")))


    ;; only numbers and hiphens
    (is (thrown? Exception (format-date* "2012:02-31")))

    ;; date size 8 or 6 or 4
    (is (thrown? Exception (format-date* "01")))
    (is (thrown? Exception (format-date* "1")))
    (is (thrown? Exception (format-date* "20111")))
    )

  (testing "formatting only-time"
    (is (= "235959" (format-time* "23:59:59")))
    (is (= "235900" (format-time* "23:59")))
    (is (= "230000" (format-time* "23")))
    (is (= "235959" (format-time* "235959")))

    ;; only numbers and hiphens
    (is (thrown? Exception (format-time* "23:59-59")))

    (is (thrown? Exception (format-time* "1")))
    (is (thrown? Exception (format-time* "1")))
    (is (thrown? Exception (format-time* "111")))

    )
  (testing "formatting time zone"
    (is (= "235959" (format-time-zone* "23:59:59")))
    (is (= "235900" (format-time* "23:59")))
    (is (= "230000" (format-time* "23")))
    (is (= "235959" (format-time* "235959")))

    ;; only numbers and hiphens
    (is (thrown? Exception (format-time* "23:59-59")))

    (is (thrown? Exception (format-time* "1")))
    (is (thrown? Exception (format-time* "1")))
    (is (thrown? Exception (format-time* "111")))

    )



  )

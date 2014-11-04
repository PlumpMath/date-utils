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
    (is (thrown? Exception (format-time* "111"))))

  (testing "formatting time zone"
    (is (= "+235959" (format-time-zone* "+23:59:59")))
    (is (= "-230000" (format-time-zone* "-23")))
    (is (= "Z" (format-time-zone* "Z"))))


  (testing "formatting date-time"
    (is (= "20120530T121205+235959" (format-date-time "2012-05-30T12:12:05+23:59:59")))
    (is (= "20120530T121205-235959" (format-date-time "2012-05-30T12:12:05-23:59:59")))
    (is (= "20120530T121205Z" (format-date-time "2012-05-30T12:12:05Z")))
    (is (= "20120530T121205Z" (format-date-time "2012-05-30T12:12:05")))


    (is (= "20120501T121200+235900" (format-date-time "2012-05T12:12+23:59")))
    (is (= "20120101T120000+230000" (format-date-time "2012T12+23")))

    ;; you can add timezone without time
    (is (thrown? Exception  (format-date-time "2012+23"))))



  (testing "parsing date-time"
    (is (= "20120530T121205Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30T12:12:05"))))
    (is (= "20120530T121200Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30T12:12"))))
    (is (= "20120530T120000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30T12"))))
    (is (= "20120530T000000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30"))))

;; trying timezone +/-
    (is (= "20120530T110000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30T12+01"))))
    (is (= "20120530T130000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30T12-01"))))

    (is (= "20120530T103000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30T12+01:30"))))
    (is (= "20120530T133000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30T12-01:30"))))

    (is (= "20120530T102930Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30T12+01:30:30"))))
    (is (= "20120530T133030Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "2012-05-30T12-01:30:30"))))


    ;; plain format

    (is (= "20120530T121205Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530T121205"))))
    (is (= "20120530T121200Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530T1212"))))
    (is (= "20120530T120000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530T12"))))
    (is (= "20120530T000000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530"))))

;; plain format trying timezone +/-
    (is (= "20120530T110000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530T12+01"))))
    (is (= "20120530T130000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530T12-01"))))

    (is (= "20120530T103000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530T12+0130"))))
    (is (= "20120530T133000Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530T12-0130"))))

    (is (= "20120530T102930Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530T12+013030"))))
    (is (= "20120530T133030Z"
           (tf/unparse (:basic-date-time-no-ms tf/formatters) (parse-date* "20120530T12-013030"))))




    )
  (testing "parsing date-time exceptions"
    (is (thrown? Exception  (parse-date* "2012-05:30T12:12:05"))) ;; colons in date
    (is (thrown? Exception  (parse-date* "2012-05-3012:12:05")));; without T
    (is (thrown? Exception  (parse-date* "2012-05-30A12:12:05")));; not allowed letter A
    (is (thrown? Exception  (parse-date* "2012-05-30TA2:12:05")));; not allowed letter A
    (is (thrown? Exception  (parse-date* "2012-05-30T0:12:05Z"))) ;; hour must have 2 digits
    (is (thrown? Exception  (parse-date* "2012-05-30Z"))) ;; date format only numbers and hiphens



    )


  (testing "parse dur"

    (is (= {tc/years 5} (parse-dur* "5y")))
    (is (= {tc/years 50} (parse-dur* "50y")))
    (is (= {tc/years 5
            tc/months 4} (parse-dur* "5y4M")))
    (is (= {tc/years 5
            tc/months 4
            tc/days 3} (parse-dur* "5y4M3d")))
    (is (= {tc/years 5
            tc/months 4
            tc/days 3
            } (parse-dur* "5y4M3d")))

    (is (= {tc/weeks 50} (parse-dur* "50W")))
    (is (= {tc/hours 10} (parse-dur* "T10H")))
    (is (= {tc/hours 10
            tc/minutes 20
            tc/seconds 30} (parse-dur* "T10H20M30S")))
    (is (= {tc/years 5
            tc/months 4
            tc/days 3
            tc/hours 10
            tc/minutes 20
            tc/seconds 30
            } (parse-dur* "5y4M3dT10H20M30S")))


    (is (thrown? Exception (parse-dur* "5y4M3"))) ;; incomplete value
    (is (thrown? Exception (parse-dur* "5Y4M5M"))) ;; duplicated keys is unordered data too

    )


  (testing "applying dur to date"
    (let [date (parse-date* "2012-05-30T12:12:05")
          date-1 (parse-date* "2012-05-01T12:12:05")
          ]
     (is (= "20170530T121205Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "5y" date))))
     (is (= "20170630T121205Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "5y1M" date))))
     (is (= "20170630T121205Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "5y1M" date))))
     (is (= "20140630T121205Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "2y1M" date))))
     (is (= "20140701T121205Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "2y1M1D" date))))
     (is (= "20140702T121205Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "2y1M2D" date))))

     (is (= "20120530T121235Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "T30S" date))))
     (is (= "20120530T124235Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "T30M30S" date))))

     (is (= "20120530T224235Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "T10H30M30S" date))))

     (is (= "20120531T224235Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "1DT10H30M30S" date))))
     (is (= "20120602T224235Z"
            (tf/unparse (:basic-date-time-no-ms tf/formatters)
                        (apply-dur-to-date "1M1DT10H30M30S" date-1)))))))

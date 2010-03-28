
(ns synpl.testspd)

(defn testit []
  (println "hello"))

(defstruct person :fname :lname :age :height :address :phone :email)

(defn make-person [fname lname age height address phone email]
  (vector :person fname lname age height address phone email))

(defn person-lname [person]
  (person 2))

(defn person-email [person]
  (person 7))

(defn testvec []
  (let [person (make-person "John" "Smith" 23 190 "Boar Rd. 1" "333-333" "jsmith@yahoo.com")]
    (person-lname person)
    (person-email person)))

(defn testmap []
  (let [person (struct person "John" "Smith" 23 190 "Boar Rd. 1" "333-333" "jsmith@yahoo.com")]
    (:lname person)
    (:email person)))
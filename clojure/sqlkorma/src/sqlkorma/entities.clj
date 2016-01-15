(ns sqlkorma.entities
    (:use korma.core)
)
(require '[clojure.string :as str])

(declare user email phone)

(defentity user
    (pk :id)
    (table :users)
    (entity-fields :name)
    (has-many email)
    (has-one phone)
)

(defentity email
    (pk :id)
    (table :emails)
    (entity-fields :email)
    (belongs-to user)

    ; (prepare (fn [{last :email :as v}]
    ;          (if last
    ;            (assoc v :email (str/upper-case last)) v)))

    (transform (fn [{first :email :as v}]
               (if first
                 (assoc v :email (str/upper-case first)) v)))
)

(defentity phone
    (pk :id)
    (table :phones)
    (entity-fields :phone)
    (belongs-to user))
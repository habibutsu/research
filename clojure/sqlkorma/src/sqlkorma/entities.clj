(ns sqlkorma.entities
    (:use korma.core)
)

(declare user email phone)

(defentity user
    (pk :id)
    (table :users)
    (entity-fields :id :name) ;; default fields for selects

    (has-many email)
    (has-one phone)
)

(defentity email
    (table :emails)
    (entity-fields :id :email)
    (belongs-to user))

(defentity phone
    (table :phones)
    (entity-fields :id :phone)
    (belongs-to user))
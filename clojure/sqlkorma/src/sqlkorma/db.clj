(ns sqlkorma.db
    (:use korma.db)
)

(defdb testdb (postgres {
    :host "127.0.0.1"
    :user "dbuser"
    :password "dbpassword"
    :db "testdb"
}))

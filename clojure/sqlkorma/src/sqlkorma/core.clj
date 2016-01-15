(ns sqlkorma.core
    (:use
        korma.db
        korma.core
        sqlkorma.db
        sqlkorma.entities)
    (:gen-class))

(defn -main
    [& args]
    (setup-db)
    (let [
        user_obj (insert user (values {:name "user", :email "null@domain"}))
        email1_obj (insert email (
                values {
                    :users_id (user_obj :id)
                    :email "email1@domain"}))
        email2_obj (insert email (
                values {
                    :users_id (user_obj :id)
                    :email "email2@domain"}))
        phone_obj (insert phone (
                values {
                    :users_id (user_obj :id)
                    :phone "+375297777777"}))
        ]
        (println "user: " user_obj)
        (println "email 1: " email1_obj)
        (println "email 2: " email2_obj)
        (println "phone: " phone_obj)
    )
    (println "using 'with' (dry-run): "
        (dry-run (select user (with-batch email) (with-batch phone))))
    (let [users (select user (fields :id) (with-batch email) (with-batch phone))]
        (println "using 'with': " users)
    )

    (defn sql-join [] (
        select user
            (fields :email :emails.email)
            (join email (= :emails.users_id :id)))
    )
    (let [users (sql-join)]
        (println "using 'join' (sql): " (sql-only (sql-join)))
        (println "using 'join': " users)
    )
    (println "raw sql: "
        (exec-raw ["SELECT * FROM emails"] :results) 
    )
)

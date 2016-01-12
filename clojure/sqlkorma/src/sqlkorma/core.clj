(ns sqlkorma.core
    (:use
        korma.db
        korma.core
        sqlkorma.db
        sqlkorma.entities
        sqlkorma.db_schema)
    (:gen-class))

(defn -main
    [& args]
    (setup-db)
    (let [
        user_obj (insert user (values {:name "user"}))
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
    (println "dry-run (batch): "
        (dry-run (select user (with-batch email) (with-batch phone))))
    (let [users (select user (with-batch email) (with-batch phone))]
        (println "using 'with': " users)
    )
    ; (let [
    ;         users (
    ;             select user
    ;             (fields :email.email)
    ;             (join email (= :email.users_id :id)))]
    ;     (println "using joins: "users)
    ; )
)

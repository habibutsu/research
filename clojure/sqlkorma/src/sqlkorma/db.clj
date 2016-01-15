(ns sqlkorma.db
    (:use
        korma.db
        korma.core)
)

(defdb testdb (postgres {
    :host "127.0.0.1"
    :user "dbuser"
    :password "dbpassword"
    :db "testdb"
}))

(def schema [
    "drop table if exists \"emails\";"
    "drop table if exists \"phones\";"
    "drop table if exists \"users\";"
    "create table \"users\" (
        \"id\" serial primary key,
        \"name\" varchar(128),
        \"email\" varchar(128),
        \"country\" varchar(128)
    );"

   "create table \"emails\" (
        \"id\" serial primary key,
        \"users_id\" integer,
        \"email\" varchar(100),
        foreign key (\"users_id\") references \"users\"(\"id\"));"

    "create table \"phones\" (
        \"id\" serial primary key,
        \"users_id\" integer,
        \"phone\" varchar(100),
        foreign key (\"users_id\") references \"users\"(\"id\"));"
])


(defn setup-db []
    (with-db testdb
        (dorun (map exec-raw schema)))
)
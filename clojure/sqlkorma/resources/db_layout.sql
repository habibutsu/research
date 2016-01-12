DROP TABLE IF EXISTS emails;

DROP TABLE IF EXISTS users;

CREATE TABLE users (
    id serial,
    name character varying(128) NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE email (
    id serial,
    users_id integer NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    email character varying(128) NOT NULL,
    PRIMARY KEY (id)
);
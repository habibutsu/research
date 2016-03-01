package entities

import javax.persistence.{Id, GeneratedValue, GenerationType, Entity, Table}


/*
create table users (
    id serial,
    priority integer not null,
    name character varying(64) not null,
    primary key (id)
);
insert into users (priority, name) values(0, 'user1');
insert into users (priority, name) values(0, 'user2');
*/

@Entity
@Table(name = "users")
class User(var name: String = null, var priority: Int = 0) {
  def this() = this(null, 0)

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  var id: Int = _;

  override def toString ():String = {
    "<User#" + id + "(" + name + "," + priority + ")>"
  }
}
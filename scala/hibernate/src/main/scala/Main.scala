/**
 * Created by habibutsu on 01.03.16.
 */
import com.mchange.v2.c3p0.ComboPooledDataSource
import org.postgresql.util.PSQLException
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;

import entities.User

object Main extends App{

  implicit def map2Properties(map: Map[String, String]) =
    map.foldLeft(new java.util.Properties )({
      case (acc, (k, v)) =>
        acc.put(k, v)
        acc
    })

  override def main (args: Array[String]) {
    // hook for Ctrl+C
    @volatile var keepRunning = true

    sys.addShutdownHook({
      keepRunning = false
    })

    val cfg = new Configuration().setProperties(
      Map(
        "hibernate.connection.provider_class" -> "org.hibernate.connection.C3P0ConnectionProvider",
        "hibernate.connection.url" -> "jdbc:postgresql://127.0.0.1/testdb",
        "hibernate.connection.username" -> "dbuser",
        "hibernate.connection.password" -> "dbpassword",
        // c3p0 specific
        "hibernate.c3p0.initialPoolSize" -> "1"
      )).addAnnotatedClass(classOf[User])

    val factory = cfg.buildSessionFactory()
    val session = factory.openSession()

    def sample_new(){
      val user = new User("username", 4)
      val tx = session.beginTransaction();
      session.save(user)
      tx.commit();

      println("New user")
      println(user)
    }
    sample_new()

    def sample_SQLQuery(){
      val user_list = session.createSQLQuery("select * from users limit 5").list()
      println("SQL query:")
      println(user_list)
    }
    sample_SQLQuery()

    def sample_Query(){
      val query = session.createQuery("from User");
      query.setMaxResults(5);
      println("Query:")
      println(query.list())
    }
    sample_Query()

    while(keepRunning){
      print(".")
      Thread.sleep(1000)
    }
    session.flush();
    session.close();
  }

}

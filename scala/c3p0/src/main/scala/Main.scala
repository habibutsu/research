/**
 * Created by habibutsu on 02.09.15.
 */
import com.mchange.v2.c3p0.ComboPooledDataSource
import org.postgresql.util.PSQLException

object Main extends App{
  override def main (args: Array[String]) {
    // hook for Ctrl+C
    @volatile var keepRunning = true

    sys.addShutdownHook({
      keepRunning = false
    })

    val cpds = new ComboPooledDataSource()
    cpds.setDriverClass("org.postgresql.Driver")
    cpds.setJdbcUrl( "jdbc:postgresql://127.0.0.1/testdb" )
    cpds.setUser("dbuser")
    cpds.setPassword("dbpassword")
    cpds.setInitialPoolSize(1)
    cpds.setMinPoolSize(1)
    cpds.setIdleConnectionTestPeriod(10)
    // custom query for checking connection
    //cpds.setPreferredTestQuery("select 1")

    val testConnection = cpds.getConnection()

    // empty statement that used for checking connection
    val stmt1 = testConnection.createStatement();
    stmt1.setQueryTimeout( 0 );
    stmt1.executeUpdate( "" );

    // example simple query
    val stmt2 = testConnection.createStatement();
    stmt2.setQueryTimeout( 10 );
    stmt2.executeQuery("select 1+1 as result");
    val result = stmt2.getResultSet
    result.next()
    println(result.getString("result"))
 
    val stmt3 = testConnection.createStatement();
    stmt3.setQueryTimeout( 20 );
    try{
        stmt3.executeQuery("select pg_sleep(60);");
    }catch{
        case e: PSQLException => println(e)
    }

    while(keepRunning){
      print(".")
      Thread.sleep(1000)
    }
    cpds.close()
  }
}

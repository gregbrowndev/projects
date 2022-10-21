package lectures.part3FP

import scala.util.Random

object L7_Options extends App {
  val myFirstOption: Option[Int] = Some(4)
  val noOption: Option[Int] = None

  println(myFirstOption)

  // unsafe APIs
  // Given the function below that can return null values,
  // some developers may think to wrap the result in a Some
  def unsafeMethod(): String = null
  val resultBad = Some(unsafeMethod())
  // This is WRONG, since you may create Some(null) which
  // defeats the whole point of Some - it should always be valid!
  // Instead, you should use:
  val resultGood = Option(unsafeMethod()) // Some or None
  println(resultGood)

  // Chained methods
  def backupMethod(): String = "A valid result"
  val chainedResult = Option(unsafeMethod()).orElse(Option(backupMethod()))

  // REDESIGN unsafe APIs to return Option
  def betterUnsafeMethod(): Option[String] = None
  def betterBackupMethod(): Option[String] = Some("A valid result")
  val betterChainedResult = betterUnsafeMethod() orElse betterBackupMethod()

  // functions on Option
  println(myFirstOption.isEmpty)
  println(myFirstOption.get) // Unwraps option's value. UNSAFE - DO NOT USE THIS

  // map, flatMap, filter
  println(myFirstOption.map(_ * 2))
  println(myFirstOption.filter(x => x > 10))
  println(myFirstOption.flatMap(x => Option(x * 10)))

  // for-comprehensions
  // Exercise:
  // You're given an API from some other programmers. The Connection
  // class below simulates a faulty connection - sometimes when you try
  // to create a Connection you receive None.
  // Additionally, the config Map that contains the host and port used to
  // create a connection is provided by some external config file, and it
  // you cannot guarantee the keys "host" and "port" exist in the map!
  //
  // The task is to establish a connection!

  val configRaw: Map[String, String] = Map(
    // fetched from elsewhere
    "host" -> "176.45.36.1",
    "port" -> "80"
  )

  class Connection {
    def status = "Connected" // connect to some server
  }
  object Connection {
    val random = new Random(System.nanoTime())

    // simulates faulty connection
    def apply(host: String, port: String): Option[Connection] =
      if (random.nextBoolean()) Some(new Connection)
      else None
  }

  // Our program
  case class Config(host: String, port: String)
  def parseConfig(config: Map[String, String]): Option[Config] = {
    // My imperative implementation (not used to using flatMap!)
    // if (!(config.contains("host") && config.contains("port"))) None
    // else
    //   Some(Config(host = config.get("host").get, port = config.get("port").get))

    // Implementation using flatMap
    val host = config.get("host")
    val port = config.get("port")
    host.flatMap(h => port.flatMap(p => Some(Config(h, p))))

    // Even better impl using for-comprehensions!
    for {
      host <- config.get("host")
      port <- config.get("port")
    } yield Config(host, port)
  }

  val config = parseConfig(configRaw)

  // if (config.isEmpty) throw RuntimeException("Could not parse config")
  // else {
  //   val connection = getConnection(config.get.host, config.get.port)
  //   println(connection)
  // }
  val connection = config.flatMap(c => Connection(c.host, c.port))
  val connectionStatus = connection.map(c => c.status)
  println(connectionStatus)

  // Better impl using for-comprehensions
  val forConnectionStatus = for {
    c <- config
    conn <- Connection(c.host, c.port)
  } yield conn.status
  forConnectionStatus.foreach(println)

  // Note: I was a bit off understanding what the exercise wanted to accomplish.
  // However, using flatMap in place of if(condition) is definitely unnatural to me!
}

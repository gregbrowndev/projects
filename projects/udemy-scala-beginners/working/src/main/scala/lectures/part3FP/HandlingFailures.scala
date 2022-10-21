package lectures.part3FP

import scala.util.{Failure, Random, Success, Try}

object HandlingFailures extends App {
  val aSuccess = Success(3)
  val aFailure = Failure(new RuntimeException("BAD!"))

  println(aSuccess)
  println(aFailure)

  def unsafeMethod(): String = throw new RuntimeException("NO THANK YOU")

  val potentialFailure = Try(unsafeMethod())
  println(potentialFailure)

  // syntax sugar
  val anotherPotentialFailure = Try {
    // code that might throw
  }

  // utilities
  println(potentialFailure.isSuccess)
  println(potentialFailure.isFailure)

  // orElse
  def backupMethod(): String = "A valid result"
  val fallbackTry = Try(unsafeMethod()).orElse(Try(backupMethod()))
  println(fallbackTry)

  // Design your API to return Try if they can throw!
  def betterUnsafeMethod(): Try[String] = Failure(
    new RuntimeException("Something went wrong")
  )
  def betterBackupMethod(): Try[String] = Success("An even better result")
  val betterFallback = betterUnsafeMethod() orElse betterBackupMethod()
  println(betterFallback)

  // map, flatMap, filter
  println(aSuccess.map(_ * 2))
  println(aSuccess.flatMap(x => Success(x * 10)))
  println(aSuccess.filter(_ > 10))
  // Failure(java.util.NoSuchElementException: Predicate does not hold for 3)
  // filter is the only thing that can turn a Success into a Failure

  /* Exercise
   * You're given a class Connection that has a poorly
  designed API. The connection is flaky and can throw an exception
  instead of returning the HTML.

   You are also provided a HttpService object that provides a
   Connection. However, this method can also throw.

  Objective: print the HTML page from the connection or the failure
   * */
  val hostname = "localhost"
  val port = "8080"
  def renderHTML(page: String): Unit = println(page)

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html>...</html>"
      else throw new RuntimeException("Connection interrupted")
    }
  }

  object HttpService {
    val random = new Random(System.nanoTime())

    def getConnection(host: String, port: String): Connection = {
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Someone else took the port")
    }
  }

  // Run a few times to see the successes/failures
  val result = for {
    conn <- Try(HttpService.getConnection(hostname, port))
    page <- Try(conn.get(s"http://$hostname:$port/home"))
  } yield page
  println("Exercise:")
  println(result) // Success(<html>...</html>)
  result.map(renderHTML) // <html>...</html>

  /* The equivalent imperative code would look like:

    try {
      connection = HttpService.getConnection(hostname, port)
      try {
        page = connection.get(s"http://$hostname:$port/home"))
        renderHTML(page)
      } catch (/* some other exception */)
      } catch (/* another exception */)
    }
   */
}

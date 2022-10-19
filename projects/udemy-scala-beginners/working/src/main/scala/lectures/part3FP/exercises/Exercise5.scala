package lectures.part3FP.exercises

import scala.annotation.tailrec

object Exercise5 extends App {
  /* Exercises for Tuples and Maps

        1. What would happen if you had a map with duplicate keys and then you ran a map function
           that applies toLowerCase to the key?
        2. Overly simplified social network based on maps
            Person = String
             - add a Person to the network
             - remove
             - friend (mutual)
             - unfriend (mutual)

            stats:
             - number of friends of a person
             - person with the most friends
             - how many people have NO friends?
             - if there is a social connection between two people (direct or not)
   */

  // 1
  val aMap = Map[String, Int]("greg" -> 30, "GREG" -> 25, "Greg" -> 44)
  println(aMap.map(pair => pair._1.toLowerCase() -> pair._2))
  // The above prints: Map(greg -> 44). Clearly, the last occurrence is kept

  // 2 - social network
  type Network = Map[String, Set[String]]
  object Network {
    def apply(): Network = Map()
    def add(n: Network, person: String): Network = {
      n + (person -> Set())
    }
    def remove(n: Network, person: String): Network = {
      n(person).foldLeft(n)((n, friend) => unfriend(n, person, friend)) - person
    }
    def friend(n: Network, sender: String, receiver: String): Network = {
      val senderFriends = n(sender) + receiver
      val receiverFriends = n(receiver) + sender
      n + (sender -> senderFriends) + (receiver -> receiverFriends)
    }
    def unfriend(n: Network, sender: String, receiver: String): Network = {
      val senderFriends = n(sender) - receiver
      val receiverFriends = n(receiver) - sender
      n + (sender -> senderFriends) + (receiver -> receiverFriends)
    }

    // stats
    def numberOfFriends(n: Network, person: String): Int = {
      if (!n.contains(person)) 0
      else n(person).size
    }
    def mostFriends(n: Network): String = {
      n.maxBy(_._2.size)._1
    }
    def numberWithNoFriends(n: Network): Int = {
      n.count((person, friends) => friends.isEmpty)
    }
    def hasConnection(
        n: Network,
        person: String,
        target: String
    ): Boolean = {
      @tailrec
      def _bfs(seen: Set[String], currentLevel: Set[String]): Boolean = {
        if (currentLevel.isEmpty) false
        else if (currentLevel.contains(target)) true
        else {
          // build set of all unseen people at the next level in the graph
          val nextLevel =
            currentLevel.foldLeft(Set[String]())((s, currentPerson) =>
              s ++ n(currentPerson)
            ) -- seen
          _bfs(seen ++ currentLevel, nextLevel)
        }
      }

      // add person to currentLevel so we can evaluate hasConnection(person, person), i.e. person with itself
      _bfs(Set(person), n(person) + person)
    }
  }

  import Network._
  var network = Network()
  network = add(network, "Greg")
  network = add(network, "Lewis")
  network = add(network, "Mary")
  network = add(network, "Luke")
  println(network)

  network = friend(network, "Greg", "Lewis")
  network = friend(network, "Greg", "Mary")
  println(network)

  println(numberOfFriends(network, "Greg"))
  println(numberOfFriends(network, "Lewis"))
  println(mostFriends(network))
  println(numberWithNoFriends(network))

  network = remove(network, "Lewis")
  println(network)

  network = unfriend(network, "Greg", "Mary")
  println(network)
  println(numberWithNoFriends(network))

  network = add(network, "Lewis")
  network = friend(network, "Greg", "Mary")
  network = friend(network, "Mary", "Luke")
  println(network)
  println(hasConnection(network, "Greg", "Luke"))
  println(hasConnection(network, "Greg", "Lewis"))
}

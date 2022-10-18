package lectures.part3FP.exercises

import scala.collection.mutable.{HashSet, Queue}
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
  type Friends = Set[String]
  type Graph = Map[String, Friends]

  case class Network(graph: Graph)
  object Network {
    def apply() = new Network(graph = Map())
    def add(n: Network, person: String): Network = {
      val newGraph = n.graph + (person -> Set())
      n.copy(graph = newGraph)
    }
    def remove(n: Network, person: String): Network = {
      // 1. This is not too efficient because it maps all values in the network.
      // Instead, recursively unfriend each person in the person to remove's friend list
      // val newGraph =
      //   (network.graph - person).mapValues(_ - person).toMap
      // network.copy(graph = newGraph)

      // 2. Better implementatin
      // def removeAux(network: Network, friends: Friends): Network = {
      //   if (friends.isEmpty) network
      //   else removeAux(unfriend(network, person, friends.head), friends.tail)
      // }
      
      // val friends = n.graph(person)
      // val newGraph = removeAux(n, friends).graph - person
      // n.copy(graph = newGraph)
      
      // 3. Most concise implementation
      val newGraph = n.graph(person).foldLeft(n)((n, friend) => unfriend(n, person, friend)).graph - person
      n.copy(graph = newGraph)

      
    }
    def friend(n: Network, sender: String, receiver: String): Network = {
      val graph = n.graph
      val senderFriends = graph(sender) + receiver
      val receiverFriends = graph(receiver) + sender
      val newGraph =
        graph + (sender -> senderFriends) + (receiver -> receiverFriends)
      n.copy(graph = newGraph)
    }
    def unfriend(
        n: Network,
        sender: String,
        receiver: String
    ): Network = {
      val graph = n.graph
      val senderFriends = graph(sender) - receiver
      val receiverFriends = graph(receiver) - sender
      val newGraph =
        graph + (sender -> senderFriends) + (receiver -> receiverFriends)
      n.copy(graph = newGraph)
    }

    // stats
    def numberOfFriends(n: Network, person: String): Int = {
      if (!n.graph.contains(person)) 0
      else n.graph(person).size
    }
    def mostFriends(n: Network): String = {
      n.graph.maxBy(_._2.size)._1
    }
    def numberWithNoFriends(n: Network): Int = {
      n.graph.count((person, friends) => friends.isEmpty)
    }
    def hasConnection(
        n: Network,
        person: String,
        target: String
    ): Boolean = {
      def bfs(g: Graph, person: String, target: String): Boolean = {
        // Solution 1:
        // @tailrec
        // def _bfs(seen: Set[String], queue: Set[String]): Boolean = {
        //     if (queue.isEmpty) false
        //     else {
        //       val currentPerson = queue.head
        //       if (currentPerson == target) true
        //       else if (seen.contains(currentPerson)) _bfs(seen, queue.tail)
        //       else _bfs(seen + currentPerson, queue ++ g(currentPerson))
        //     }
        // }

        @tailrec
        def _bfs(seen: Set[String], currentLevel: Set[String]): Boolean = {
            if (currentLevel.isEmpty) false
            else if (currentLevel.contains(target)) true
            else {
              // build set of all unseen people at the next level in the graph
              val nextLevel = currentLevel.foldLeft(Set[String]())((s, currentPerson) => s ++ g(currentPerson)) -- seen
              _bfs(seen ++ currentLevel, nextLevel)
            }
        }
        
        // add person to currentLevel so we can evaluate hasConnection(person, person), i.e. person with itself
        _bfs(Set(person), g(person) + person)
      }

      bfs(n.graph, person, target)
    }
  }

  var network = Network()
  network = Network.add(network, "Greg")
  network = Network.add(network, "Lewis")
  network = Network.add(network, "Mary")
  network = Network.add(network, "Luke")
  println(network)

  network = Network.friend(network, "Greg", "Lewis")
  network = Network.friend(network, "Greg", "Mary")
  println(network)

  println(Network.numberOfFriends(network, "Greg"))
  println(Network.numberOfFriends(network, "Lewis"))
  println(Network.mostFriends(network))
  println(Network.numberWithNoFriends(network))

  network = Network.remove(network, "Lewis")
  println(network)

  network = Network.unfriend(network, "Greg", "Mary")
  println(network)
  println(Network.numberWithNoFriends(network))
  
  network = Network.add(network, "Lewis")
  network = Network.friend(network, "Greg", "Mary")
  network = Network.friend(network, "Mary", "Luke")
  println(network)
  println(Network.hasConnection(network, "Greg", "Luke"))
  println(Network.hasConnection(network, "Greg", "Lewis"))
}

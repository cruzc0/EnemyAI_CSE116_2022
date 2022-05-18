package tests

import game.enemyai.{AIPlayer, PlayerLocation}
import game.lo4_data_structures.linkedlist.LinkedListNode
import game.maps.GridLocation
import org.scalatest._

class Task1 extends FunSuite {


  test("locatePlayer") {

    val player: AIPlayer = new AIPlayer("player1")
    var list: LinkedListNode[PlayerLocation] = new LinkedListNode[PlayerLocation](
      new PlayerLocation(3.0, 4.0, "player1"), null
    )
    list = new LinkedListNode(
      new PlayerLocation(1.0,8.0,"player2"), list
    )
    list = new LinkedListNode(
      new PlayerLocation(13.0,8.0,"player3"), list
    )
    list = new LinkedListNode(
      new PlayerLocation(132.0,8.0,"player4"), list
    )

    val actual: PlayerLocation = player.locatePlayer("player2",list)
    assert(actual.playerId == "player2")
    assert(Math.abs(actual.x-1.0) < 0.001)
    assert(Math.abs(actual.y-8.0) < 0.001)

  }

  test("closestPlayer") {
    val player: AIPlayer = new AIPlayer("player1")
    var list: LinkedListNode[PlayerLocation] = new LinkedListNode[PlayerLocation](
      new PlayerLocation(3.0, 4.0, "player2"), null
    )
    list = new LinkedListNode(
      new PlayerLocation(300.0,40.0,"player1"), list
    )
    list = new LinkedListNode(
      new PlayerLocation(10.0,8.0,"player3"), list
    )
   val actual = player.closestPlayer(list)
    //println(actual.playerId)
    assert(actual.playerId == "player3")
    assert(Math.abs(actual.x-10.0) < 0.001)
    assert(Math.abs(actual.y-8.0) < 0.001)
    var list2: LinkedListNode[PlayerLocation] = new LinkedListNode[PlayerLocation](
      new PlayerLocation(3.0, 4.0, "player1"), null
    )
    list2 = new LinkedListNode(
      new PlayerLocation(300.0,40.0,"player2"), list2
    )
    val actual2 = player.closestPlayer(list2)
    // println(actual.playerId)
    assert(actual2.playerId == "player2")
    assert(Math.abs(actual2.x-300.0) < 0.001)
    assert(Math.abs(actual2.y-40.0) < 0.001)

    list2 = new LinkedListNode(
      new PlayerLocation(10000.0,10000.0,"player3"), list2
    )

    val actual3 = player.closestPlayer(list2)
    assert(actual3.playerId == "player2")
    assert(Math.abs(actual3.x-300.0) < 0.001)
    assert(Math.abs(actual3.y-40.0) < 0.001)
  }
  test("computePath") {
    val player: AIPlayer = new AIPlayer("player1")
    var path: LinkedListNode[GridLocation] = player.computePath(
      new GridLocation(1,3),
      new GridLocation(2,2)
    )
    var expected = new LinkedListNode[GridLocation](new GridLocation(2,2),null)
    expected = new LinkedListNode[GridLocation](new GridLocation(1,2),expected)
    expected = new LinkedListNode[GridLocation](new GridLocation(1,3),expected)
    assert(path.value.x == expected.value.x)
    assert(path.value.y == expected.value.y)
    assert(path.next.next.value.x == expected.next.next.value.x)
    assert(path.next.next.value.y == expected.next.next.value.y)
    assert(path.size() == expected.size())
  }

}

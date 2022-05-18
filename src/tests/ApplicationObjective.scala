package tests

import game.enemyai.{AIPlayer, PlayerLocation,AIGameState}
import game.lo4_data_structures.linkedlist.LinkedListNode
import game.maps.GridLocation
import scala.collection.mutable._
import scala.collection.mutable.Map
import org.scalatest._

class ApplicationObjective extends FunSuite {
  def makeGameState(): AIGameState = {
    val gameState: AIGameState = new AIGameState
    gameState.levelWidth = 10
    gameState.levelHeight = 8

    gameState.wallLocations = List(
      new GridLocation(3,1),
      new GridLocation(3,2),
      new GridLocation(3,3),
      new GridLocation(3,4),
      new GridLocation(3,5),
      new GridLocation(4,1),
      new GridLocation(5,1),
      new GridLocation(6,1),
      new GridLocation(6,2),
      new GridLocation(6,3),
      new GridLocation(6,4),
      new GridLocation(6,5)
    )
    var list = new LinkedListNode[PlayerLocation](new PlayerLocation(5.6,3.4,"1"),null)
    list = new LinkedListNode[PlayerLocation](new PlayerLocation(7.1,3.9,"2"),list)
    list = new LinkedListNode[PlayerLocation](new PlayerLocation(7.8,6.3,"3"),list)
    list = new LinkedListNode[PlayerLocation](new PlayerLocation(2.1,3.9,"4"),list)
    gameState.playerLocations = list
    gameState

  }
  test("getPath"){
    val gameState = makeGameState()

    val player: AIPlayer = new AIPlayer("1")
    val result: LinkedListNode[GridLocation] = player.getPath(gameState)
    assert(result.size() == 6)
    assert(result.value.x == 5)
    assert(result.value.y == 3)
    assert(result.next.value.x == 5)
    assert(result.next.value.y == 4)
    assert(result.next.next.value.x == 5)
    assert(result.next.next.value.y == 5)
    assert(result.next.next.next.value.x == 5)
    assert(result.next.next.next.value.y == 6)
    assert(result.next.next.next.next.value.x == 6)
    assert(result.next.next.next.next.value.y == 6)
    assert(result.next.next.next.next.next.value.x == 7)
    assert(result.next.next.next.next.next.value.y == 6)
  }
  test("player 1"){
    val gameState = makeGameState()

    val player: AIPlayer = new AIPlayer("1")
    val result: PlayerLocation = player.closestPlayerAvoidWalls(
      gameState
    )

    assert(result.playerId == "3")

  }

  test("player 2"){
    val gameState = makeGameState()

    val player: AIPlayer = new AIPlayer("2")
    val result: PlayerLocation = player.closestPlayerAvoidWalls(
      gameState
    )

    assert(result.playerId == "3")

  }


  test("player 3"){
    val gameState = makeGameState()

    val player: AIPlayer = new AIPlayer("3")
    val result: PlayerLocation = player.closestPlayerAvoidWalls(
      gameState
    )

    assert(result.playerId == "2")
  }
  test("player 4"){
    val gameState = makeGameState()

    val player: AIPlayer = new AIPlayer("4")
    val result: PlayerLocation = player.closestPlayerAvoidWalls(
      gameState
    )
    assert(result.playerId == "3")
  }



}

package tests

import game.enemyai.{AIPlayer, PlayerLocation,AIGameState}
import game.lo4_data_structures.linkedlist.LinkedListNode
import game.maps.GridLocation
import scala.collection.mutable._

import org.scalatest._

class Task3 extends FunSuite {

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
    gameState.playerLocations = new LinkedListNode[PlayerLocation](new PlayerLocation(5.6,3.4,"player1"),null)

    gameState

  }

  test("not test"){
    val state: AIGameState = makeGameState()
    val graph = state.levelAsGraph()
    val player: AIPlayer = new AIPlayer("1")
    val result: Int = player.distanceAvoidWalls(
      state,
      new GridLocation(8,1),
      new GridLocation(8,3)
    )
    var test = Queue(1,2,3,4)

    }

  test("your test"){
    val gameState = makeGameState()

    val player: AIPlayer = new AIPlayer("1")
    val result: Int = player.distanceAvoidWalls(
      gameState,
      new GridLocation(8,1),
      new GridLocation(8,3)
    )

    assert(result == 2)
  }

  test("your test 2"){
    val gameState = makeGameState()

    val player: AIPlayer = new AIPlayer("1")
    val result: Int = player.distanceAvoidWalls(
      gameState,
      new GridLocation(5,5),
      new GridLocation(7,5)
    )

    assert(result == 4)
  }

  test("your test 3"){
    val gameState = makeGameState()

    val player: AIPlayer = new AIPlayer("1")
    val result: Int = player.distanceAvoidWalls(
      gameState,
      new GridLocation(4,2),
      new GridLocation(6,0)
    )

    assert(result == 14 )
  }





}

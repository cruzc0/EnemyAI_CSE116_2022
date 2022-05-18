package game.enemyai

import game.enemyai.decisiontree.DecisionTreeValue
import game.lo4_data_structures.linkedlist.LinkedListNode
import game.lo4_data_structures.trees.BinaryTreeNode
import game.maps.GridLocation
import game.{AIAction, MovePlayer}
import scala.collection.mutable._
import java.util
//testing
class AIPlayer(val id: String) {
  def distanceAvoidWalls(state: AIGameState,location1: GridLocation,location2: GridLocation): Int ={
    val start_id: Int = (location1.y * state.levelWidth) + location1.x
    val end_id: Int = (location2.y * state.levelWidth) + location2.x
    var i = 0
    var q:Queue[Int] = Queue(start_id)
    var keep: List[List[Int]] = List(List(start_id))
    var map: Map[Double, Int] = Map()
    var end = 0
    while(end == 0) {
      i+=1
      var k = 0
      for (all <- keep) {
        k+=1
        if(k ==1){
          keep = List(List())
        }
        for (all <- all) {
          var between: List[Int] = List()
          for (all <- state.levelAsGraph().adjacencyList(all)) {
            if (q.contains(all)) {
            //println("No")
            }
            else {
              //println("Yes")
              q.enqueue(all)
              between = between :+ all
              map = map + (i.toDouble-> all)
              if(all == end_id){
                end = i
              }
              //println(map.slice(i-1,i))
            }
          }
          keep = keep :+ between
        }
      }
      keep = keep
    }
    end
  }
  // TODO: Replace this placeholder code with your own
  def locatePlayer(playerId: String, playerLocations: LinkedListNode[PlayerLocation]): PlayerLocation = {
    val list = toList(playerLocations.size(),playerLocations)
    var ret: PlayerLocation = new PlayerLocation(3, 3, "hello")
    for(all <- list){
      if(all.playerId == playerId){
        ret = all
      }
    }
    ret
  }

  def toList(length: Int,playerLocations: LinkedListNode[PlayerLocation]): List[PlayerLocation] = {
    if(length <= 0){
      List()
    }
    else {
         List(playerLocations.value) ++ toList(length-1,playerLocations.next)
    }

  }
  def toListCords(length: Int,playerLocations: LinkedListNode[PlayerLocation]): List[List[Int]] = {
    if(length <= 0){
      List()
    }
    else {
      List(List(playerLocations.value.x.floor.toInt,playerLocations.value.y.floor.toInt)) ++ toListCords(length-1,playerLocations.next)
    }

  }

  // TODO: Replace this placeholder code with your own
  def closestPlayer(playerLocations: LinkedListNode[PlayerLocation]): PlayerLocation = {
    val AIPlayer = locatePlayer(this.id,playerLocations)
    val comparator: (PlayerLocation,PlayerLocation) => Boolean = (a:PlayerLocation,b:PlayerLocation) => Math.sqrt(Math.pow((a.x-AIPlayer.x),2) + Math.pow((a.y-AIPlayer.y),2)) < Math.sqrt(Math.pow((b.x-AIPlayer.x),2) + Math.pow((b.y-AIPlayer.y),2))
    val list: List[PlayerLocation] = toList(playerLocations.size(),playerLocations)
    val sorted_list = list.sortWith(comparator)
    sorted_list(1)
  }


  // TODO: Replace this placeholder code with your own
  def computePath(start: GridLocation, end: GridLocation): LinkedListNode[GridLocation] = {
    var linked_list: LinkedListNode[GridLocation] = new LinkedListNode(end,null)
    var x_difference = end.x-start.x
    var y_difference = end.y-start.y
    var x_start = end.x
    var y_start = end.y
    for(all <- 0 until Math.abs(x_difference)){
      if(x_difference > 0){
        linked_list = new LinkedListNode(new GridLocation(x_start - 1,y_start),linked_list)
        x_start -= 1
      }
      else{
        linked_list = new LinkedListNode(new GridLocation(x_start + 1,y_start),linked_list)
        x_start += 1
      }
    }
    for(all <- 0 until Math.abs(y_difference)){
      if(y_difference > 0){
        linked_list = new LinkedListNode(new GridLocation(x_start,y_start-1),linked_list)
        y_start -= 1
      }
      else{
        linked_list = new LinkedListNode(new GridLocation(x_start,y_start+1),linked_list)
        y_start += 1
      }
    }
   linked_list
  }

  def letsSee(gameState: AIGameState,decisionTree: BinaryTreeNode[DecisionTreeValue]): AIAction ={
    val check = decisionTree.value.check(gameState)
    if(check < 0){
      letsSee(gameState,decisionTree.left)
    }
    else if(check > 0){
      letsSee(gameState,decisionTree.right)
    }
    else{
      decisionTree.value.action(gameState)
    }
  }

  // TODO: Replace this placeholder code with your own
  def makeDecision(gameState: AIGameState, decisionTree: BinaryTreeNode[DecisionTreeValue]): AIAction = {
    letsSee(gameState,decisionTree)
  }


  // TODO: Replace this placeholder code with your own
  def closestPlayerAvoidWalls(gameState: AIGameState): PlayerLocation = {
   var list = toList(gameState.playerLocations.size(),gameState.playerLocations)
    var completeList: List[PlayerLocation] = List()
    //println("ID: " + this.id)
    val player = locatePlayer(this.id,gameState.playerLocations)
    for(all <- list){
      if(all.playerId != player.playerId){
        completeList = completeList :+ all
      }
    }
    var list2: List[Int] = List()
    val player2: AIPlayer = new AIPlayer("1")
    for(all <- completeList) {
      list2 = list2 :+ player2.distanceAvoidWalls(gameState, new GridLocation(player.x.floor.toInt, player.y.floor.toInt), new GridLocation(all.x.floor.toInt, all.y.floor.toInt)
      )
    }
    var sorted = list2.sorted
    var return_it: PlayerLocation = player
    var i=0
    for(all <- list2){
      if(all == sorted(0)){
        return_it = completeList(i)
      }
      i+=1
    }
    return_it
  }

  // TODO: Replace this placeholder code with your own
  def getPath(gameState: AIGameState): LinkedListNode[GridLocation] = {
    var ret: List[LinkedListNode[GridLocation]] = List()
    val player_id: Int = locatePlayer(this.id,gameState.playerLocations).y.floor.toInt * gameState.levelWidth + locatePlayer(this.id,gameState.playerLocations).x.floor.toInt
    val list_cords: List[List[Int]] = toListCords(gameState.playerLocations.size(),gameState.playerLocations)
    for(all <- list_cords){
      val goal: Int = (all(1)*gameState.levelWidth) + all(0)
      if(all != List(locatePlayer(this.id,gameState.playerLocations).x.floor.toInt,locatePlayer(this.id,gameState.playerLocations).y.floor.toInt)){
        ret = ret :+ getPathSingle(gameState,goal,player_id,new GridLocation(locatePlayer(this.id,gameState.playerLocations).x.floor.toInt,locatePlayer(this.id,gameState.playerLocations).y.floor.toInt),new GridLocation(all(0),all(1)))
      }
    }
    var ret_sorted = ret.sortWith((a:LinkedListNode[GridLocation],b:LinkedListNode[GridLocation]) => a.size() < b.size())
    ret_sorted(0)
  }
  def getPathSingle(gameState: AIGameState,end_id1: Int,begin_id1: Int,grid1: GridLocation, grid2: GridLocation): LinkedListNode[GridLocation] = {
    val player = locatePlayer(this.id,gameState.playerLocations)
    val list: List[List[Int]] = avoid(gameState,grid1, grid2)
    val end_id = end_id1
    val begin_id = begin_id1
    val raw_list: List[Int] = recursive(list,end_id,begin_id)
    var finished_list: List[GridLocation] = List()
    for(all <- raw_list){
      finished_list = finished_list :+ new GridLocation(gameState.levelAsGraph().nodes(all).x,gameState.levelAsGraph().nodes(all).y)
    }
    val reversed_list = finished_list.reverse
    //println("reversed: " + reversed_list)
    var linked_list = new LinkedListNode[GridLocation](gameState.levelAsGraph().nodes(end_id), null)
    linked_list = new LinkedListNode[GridLocation](finished_list(0),linked_list)
    //linked_list = new LinkedListNode[GridLocation](gameState.levelAsGraph().nodes(38),linked_list)
    for(all <- finished_list.indices){
      if(all != 0){
        linked_list = new LinkedListNode[GridLocation](finished_list(all),linked_list)
      }
    }
    linked_list
  }
  def find(testing:List[List[Int]],length:Int,desired:Int): List[Int] = {
    if(testing(length-1)(1) == desired){
      testing(length-1)
    }
    else{
      find(testing,length-1,desired)
    }
  }
  def recursive(testing: List[List[Int]],new1:Int,final_id:Int): List[Int] ={
    if(new1 == final_id){
      List()
    }
    else{
      List(find(testing,testing.size,new1)(0)) ++ recursive(testing,find(testing,testing.size,new1)(0),final_id)
    }
  }
  def avoid(state: AIGameState,location1: GridLocation,location2: GridLocation): List[List[Int]]={
    val start_id: Int = (location1.y * state.levelWidth) + location1.x
    val end_id: Int = (location2.y * state.levelWidth) + location2.x
    var i = 0
    var q:Queue[Int] = Queue(start_id)
    var keep: List[List[Int]] = List(List(start_id))
    var map: List[List[Int]] = List()
    var end = 0
    while(end == 0) {
      i+=1
      var k = 0
      for (all <- keep) {
        k+=1
        if(k ==1){
          keep = List(List())
        }
        for (all2 <- all) {
          var between: List[Int] = List()
          for (all3 <- state.levelAsGraph().adjacencyList(all2)) {
            if (q.contains(all3)) {
              //println("No")
            }
            else {
              //println("Yes")
              q.enqueue(all3)
              between = between :+ all3
              //val all2 = all2
              //val all3 = all3
              map = map :+ List(all2,all3)
              if(all3 == end_id){
                end = i
              }

            }
          }
          keep = keep :+ between
        }
      }
      keep = keep
    }
    map
  }

}


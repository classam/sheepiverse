import akka.actor._
import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.duration.TimeUnit

case object Greet
case class WhoToGreet(who: String)
case class Greeting(message: String)

abstract class TimingEvent
case class Tick() extends TimingEvent
case class Collision() extends TimingEvent
case class Think(state:Snapshot) extends TimingEvent
case class Response(token_id:String, updates:List[StateUpdate]) extends TimingEvent

class State extends Actor {
  val stateGrid = new StateGrid(15, 15, Game.createToken, Dirt())
  for( i <- 0 until 15){
    for( j <- 0 until 15){
      if(i % 5 == 0 && j % 5 == 0){stateGrid.stateUpdate(CreateToken("sheep", i, j))}
      if(i % 5 == 1 && j % 5 == 1){stateGrid.stateUpdate(CreateToken("grass", i, j))}
    }
  }

  //stateGrid.stateUpdate(CreateToken("sheep", 5, 5))
  //stateGrid.stateUpdate(CreateToken("grass", 5, 5))

  val map:mutable.HashMap[String, ActorRef] = new mutable.HashMap[String, ActorRef]()
  val hasResponded:mutable.HashMap[String, Boolean] = new mutable.HashMap[String, Boolean]()
  val output:ActorRef = context.actorOf(Props[Output], "output")

  //create souls
  actorUpdate()

  //create snapshot
  var snapshot = stateGrid.toSnapshot()

  def actorUpdate(){
    val actors = stateGrid.tokenBase.tags.get(Token.actor)
    if (actors.isDefined)
    {
      actors.get.foreach(token => {
        val maybe:Option[ActorRef] = map.get(token.id)
        if( !maybe.isDefined ){
          val actor_ref:ActorRef = context.actorOf(Soul.props(token))
          map.update(token.id, actor_ref)
          hasResponded.update(token.id, true)
        }
      })
    }
  }

  def sendMessageToToken(token_id:String, event:Any){
    val maybe = map.get(token_id)
    if(maybe.isDefined){
      maybe.get ! event
    }
  }

  def successfulStateUpdate(stateUpdate:StateUpdate) = stateUpdate match {
    case CreateToken(token_name, x, y) => {
      actorUpdate()
    }
    case DestroyToken(token_id) => {
      sendMessageToToken(token_id, PoisonPill)
      map.remove(token_id)
    }
    case _ => {}
  }

  def failedStateUpdate(stateUpdate:StateUpdate):Unit = stateUpdate match {
    case MoveToken(token_id, x, y) => {
      sendMessageToToken(token_id, Collision())
    }
    case _ => {}
  }

  def receive = {
    case Response(token_id, updates) => {
      hasResponded.update(token_id, true)
      updates.foreach((update) => {
        var attempt = stateGrid.stateUpdate(update)
        if(attempt){
          successfulStateUpdate(update)
        }
        else{
          failedStateUpdate(update)
        }
      })
    }
    case Tick() => {
      val t0 = System.nanoTime()
      snapshot = stateGrid.toSnapshot()
      output ! Think(snapshot)
      // send every actor who has responded to a tick... a tick
      map.foreach {
        case (id: String, actor: ActorRef) => {
          var responded = hasResponded.get(id)
          if (responded.isDefined && responded.get) {
            actor ! Think(snapshot)
            hasResponded.update(id, false)
          }
          else {
            println("I haven't heard back from " + id + " yet")
          }
        }
      }
      val t1 = System.nanoTime()
      println("Tick completed in: " + (t1 - t0)/1000000 + " ms")
    }
  }
}

class Output extends Actor {
  def receive = {
    case Think(snapshot) => {
      val t0 = System.nanoTime()
      println(snapshot.toString)
      val t1 = System.nanoTime()
      println("Print completed in: " + (t1 - t0)/1000000 + " ms")
    }
  }
}


object HelloAkkaScala extends App {

  val system = ActorSystem("sheepworld")

  val state = system.actorOf(Props[State], "state")

  // Create an "actor-in-a-box"
  val inbox = Inbox.create(system)

  system.scheduler.schedule(
    300.milliseconds,
    300.milliseconds,
    state,
    Tick())(system.dispatcher)
}
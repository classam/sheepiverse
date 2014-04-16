import akka.actor.{Props, Actor}

class Soul(val token:Token) extends Actor {
  def receive = {
    case Think(snapshot) => {
      sender ! Response(token.id, token.tick(snapshot))
    }
    case Collision() => {
      token.collision()
    }
  }
}

object Soul {
  def props(token:Token):Props = Props(new Soul(token))
}
import org.scalatest._
import org.scalatest.matchers._

class StateSpec extends FlatSpec with ShouldMatchers {

  "A State" should "create actors in the grid" in {
    val stateGrid = new StateGrid(15, 15, Game.createToken, Dirt())
    stateGrid.stateUpdate(CreateToken("sheep", 5, 5))
    stateGrid.tokenGrid.get(5,5).length should be (1)
    stateGrid.toSnapshot().tokenGrid.get(5,5).length should be (1)
    stateGrid.tokenBase.tags.get(Token.actor).size should be (1)
  }

  "A State" should "create actors in the tokenbase" in {
    val stateGrid = new StateGrid(15, 15, Game.createToken, Dirt())
    stateGrid.stateUpdate(CreateToken("sheep", 5, 5))
    stateGrid.tokenBase.tags.get(Token.actor).size should be (1)
    val id:String = stateGrid.tokenBase.tags.get(Token.actor).get.last.id
    stateGrid.tokenBase.get(id).isDefined should be (true)
    stateGrid.tokenBase.get(id).get.id should be (id)
  }

  "A State" should "remove actors from the grid" in {
    val stateGrid = new StateGrid(15, 15, Game.createToken, Dirt())
    stateGrid.stateUpdate(CreateToken("sheep", 5, 5))
    val id:String = stateGrid.tokenBase.tags.get(Token.actor).get.last.id
    stateGrid.stateUpdate(DestroyToken(id))
    stateGrid.tokenGrid.get(5,5).length should be (0)
    stateGrid.toSnapshot().tokenGrid.get(5,5).length should be (0)
  }

  "A State" should "remove actors from the tokenbase" in {
    val stateGrid = new StateGrid(15, 15, Game.createToken, Dirt())
    stateGrid.stateUpdate(CreateToken("sheep", 5, 5))
    val id:String = stateGrid.tokenBase.tags.get(Token.actor).get.last.id
    stateGrid.stateUpdate(DestroyToken(id))
    stateGrid.tokenBase.tags.get(Token.actor).get.length should be (0)
    stateGrid.tokenBase.get(id).isDefined should be (false)
  }

}

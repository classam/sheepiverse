import org.scalatest._
import org.scalatest.matchers._

class TokenSpec extends FlatSpec with ShouldMatchers {

  "A Sheep" should "be an actor" in {
    val token = new Sheep(5,5)
    token.extendedTags.contains(Token.actor) should be(true)
    token.extendedTags.contains(Token.collidable) should be (true)
    token.extendedTags.contains(Token.stateful) should be (true)
  }

  "A Tokenbase" should "add a new sheep" in {
    val tokenbase = new MutableTokenBase()
    val token = new Sheep(5,5)
    tokenbase.add(token)
    tokenbase.get(token.id).get.name should be (token.name)
    tokenbase.get(token.id).get.x should be(5)
  }

  "A Tokenbase" should "still have that sheep after becoming immutable" in {
    val tokenbase = new MutableTokenBase()
    val token = new Sheep(5,5)
    tokenbase.add(token)
    tokenbase.toImmutable.get(token.id).get.name should be (token.name)
  }

  "A Tokenbase" should "maintain a list of actors, which should include sheep" in {
    val tokenbase = new MutableTokenBase()
    tokenbase.add(new Sheep(1,1))
    tokenbase.add(new Sheep(2,2))
    tokenbase.add(new Sheep(3,3))
    tokenbase.tags.get(Token.actor).get.length should be (3)
  }

  "A TokenBase" should "remove actors" in {
    val tokenbase = new MutableTokenBase()
    val sheepy = new Sheep(1,1)
    tokenbase.add(sheepy)
    tokenbase.remove(sheepy.id)
    tokenbase.get(sheepy.id).isDefined should be (false)
  }

  "A TokenBase" should "remove actors from their tags" in {
    val tokenbase = new MutableTokenBase()
    val sheepy = new Sheep(1,1)
    tokenbase.add(sheepy)
    tokenbase.remove(sheepy.id)
    tokenbase.tags.get(Token.actor).get.length should be (0)
  }

  "A Token" should "be able to tell what else shares a space on the grid with it" in {
    val tokenbase = new MutableTokenBase()
    val tokengrid = new TokenIdGrid(5,5)
    val sheepy = new Sheep(1,1)
    val grass_one = new Grass(1,1)
    val grass_two = new Grass(1,1)
    tokenbase.add(sheepy)
    tokenbase.add(grass_one)
    tokenbase.add(grass_two)
    tokengrid.set(1,1,sheepy.id)
    tokengrid.set(1,1,grass_one.id)
    tokengrid.set(1,1,grass_two.id)

    tokengrid.getStack(sheepy).length should be(2)
    tokenbase.convertTokenIds(tokengrid.getStack(sheepy)).last.name should be ("grass")
  }

}
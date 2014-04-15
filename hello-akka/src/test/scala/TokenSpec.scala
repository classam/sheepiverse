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
    val tokenbase = new TokenBase()
    val token = new Sheep(5,5)
    tokenbase.add(token)
    tokenbase.get(token.id).get.name should be (token.name)
    tokenbase.get(token.id).get.x should be(5)
  }

  "A Tokenbase" should "still have that sheep after becoming immutable" in {
    val tokenbase = new TokenBase()
    val token = new Sheep(5,5)
    tokenbase.add(token)
    tokenbase.toImmutable.get(token.id).get.name should be (token.name)
  }

  "A Tokenbase" should "maintain a list of actors, which should include sheep" in {
    val tokenbase = new TokenBase()
    tokenbase.add(new Sheep(1,1))
    tokenbase.add(new Sheep(2,2))
    tokenbase.add(new Sheep(3,3))
    tokenbase.tags.get(Token.actor).get.length should be (3)
  }

  "A TokenBase" should "remove actors" in {
    val tokenbase = new TokenBase()
    val sheepy = new Sheep(1,1)
    tokenbase.add(sheepy)
    tokenbase.remove(sheepy.id)
    tokenbase.get(sheepy.id).isDefined should be (false)
  }

  "A TokenBase" should "remove actors from their tags" in {
    val tokenbase = new TokenBase()
    val sheepy = new Sheep(1,1)
    tokenbase.add(sheepy)
    tokenbase.remove(sheepy.id)
    tokenbase.tags.get(Token.actor).get.length should be (0)
  }

}
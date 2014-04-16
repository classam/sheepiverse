import scala.collection.mutable
import scala.collection.concurrent
import scala.collection.immutable.{VectorBuilder, HashMap}

abstract class Token(var x:Int, var y:Int, val id:String){

  def this(x:Int, y:Int) = this(x, y, java.util.UUID.randomUUID.toString())

  def set(setX:Int, setY:Int):Unit = { x = setX; y = setY; }

  def name = "token"
  protected def tags:Vector[String] = Vector()
  def extendedTags:Vector[String] = {
    var vector = tags
    if(isTickable){
      vector = vector :+ Token.actor
    }
    if(isCollidable){
      vector = vector :+ Token.collidable
    }
    if(hasState){
      vector = vector :+ Token.stateful
    }
    return vector;
  }
  def symbol:String = "?"

  def hasState:Boolean = false
  def isTickable:Boolean = false
  def isCollidable:Boolean = false

  def tick(state:Snapshot):List[StateUpdate] = {require(isTickable); return List();}
  def update(updateDetails:String):Unit = {require(hasState)}
  def collision():Unit = {require(isCollidable && isTickable);}
}

object Token {
  val actor = "A"
  val collidable = "C"
  val stateful = "S"
}

trait TokenBase {
  def get(token_id:String):Option[Token]
  protected def map():Map[String, Token]
  protected def tags():concurrent.TrieMap[String, List[Token]]

  def convertTokenIds(stack:List[String]):List[Token] = {
    val builder = List.newBuilder[Token]
    stack.foreach((str) => {
      val maybe = get(str)
      if(maybe.isDefined){
        builder.+=(maybe.get)
      }
    })
    return builder.result()
  }

  override def toString:String = {
    val builder = new mutable.StringBuilder()
    map.foreach{ case (key, value) => builder.append(key + " => " + value.name + " " + value.x + "," + value.y + "\n")}
    builder.append("-----------------------\n")
    tags.foreach{ case (key, value) => builder.append(key + "=>" + value.size.toString + "\n")}
    return builder.result()
  }
}

class ImmutableTokenBase(val map:Map[String, Token], val tags:concurrent.TrieMap[String, List[Token]]) extends TokenBase {
  def get(token_id: String): Option[Token] = {
    map.get(token_id)
  }
}

class MutableTokenBase() extends TokenBase{
  private val priv_map = new mutable.HashMap[String, Token]()
  val tags = new concurrent.TrieMap[String, List[Token]]

  def map:Map[String, Token] = {return priv_map.toMap[String, Token]}

  def add(t:Token):Unit = {
    priv_map.update(t.id, t);
    t.extendedTags.foreach(s => {
      var maybe = tags.get(s)
      if (maybe.isDefined) {
        tags.update(s, t :: maybe.get)
      }
      else {
        tags.update(s, List[Token](t))
      }
    });
  }

  def update(token_id:String, update_details:String):Unit = {
    val maybe = priv_map.get(token_id)
    if( maybe.isDefined ){
      maybe.get.update(update_details)
    }
  }

  def remove(token_id:String):Unit = {
    var token = map.get(token_id);
    if( token.isDefined ){
      priv_map.remove(token_id);
      token.get.extendedTags.foreach(s => {
        var maybe = tags.get(s)
        if (maybe.isDefined){
          tags.update(s, maybe.get.filterNot( item => item.id == token_id ))
        }
      });
    }
  }

  def get(token_id:String):Option[Token] = {priv_map.get(token_id)}

  def toImmutable:ImmutableTokenBase = {
    return new ImmutableTokenBase(HashMap() ++ priv_map, tags)
  }
}

class ImmutableTokenIdGrid(val width:Int, val height:Int, val grid:ImmutableGrid[List[String]]) extends Gridlike[List[String]]{
  def get(x:Int, y:Int):List[String] = {
    return grid.get(x, y)
  }

  def getStack(token:Token):List[String] = {
    return grid.get(token.x, token.y).filter( (token_id) => token_id != token.id )
  }
}

class TokenIdGrid(val width:Int, val height:Int) extends Gridlike[List[String]] {
  private val data = new Grid[List[String]](width, height, ()=>List())

  def get(x:Int, y:Int):List[String] = {
    return data.get(x, y)
  }

  def getStack(token:Token):List[String] = {
    return data.get(token.x, token.y).filter( (token_id) => token_id != token.id )
  }

  def set(x:Int, y:Int, token_id:String):Unit = {
    data.set(x, y, token_id :: data.get(x, y))
  }

  def remove(x:Int, y:Int, token_id:String):Unit = {
    data.set(x, y, data.get(x, y).filterNot( (t_id:String) => t_id == token_id ))
  }

  def toImmutable:ImmutableTokenIdGrid = {
    return new ImmutableTokenIdGrid(width, height, data.toImmutable)
  }
}
import scala.collection.mutable
import scala.collection.concurrent
import scala.collection.immutable.{VectorBuilder, HashMap}

abstract class Token(var x:Int, var y:Int, val id:String){
  def this(x:Int, y:Int) = this(x, y, java.util.UUID.randomUUID.toString())

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

  def myStack(state:Snapshot):Vector[Token] = {
    val vectorBuilder = Vector.newBuilder[Token]
    state.tokenGrid.get(x, y).foreach((str) => {
      val maybe = state.tokenBase.get(str)
      if(maybe.isDefined){
        vectorBuilder.+=(maybe.get)
      }
    })
    return vectorBuilder.result()
  }
}

object Token {
  val actor = "A"
  val collidable = "C"
  val stateful = "S"
}

class ImmutableTokenBase(val map:Map[String, Token], val tags:concurrent.TrieMap[String, List[Token]]){
  def get(token_id:String):Option[Token] = {map.get(token_id)}

  override def toString:String = {
    val stringbuilder = new mutable.StringBuilder()
    map.foreach{ case (key, value) => stringbuilder.append(key + " => " + value.name + " " + value.x + "," + value.y + "\n")}
    stringbuilder.append("-----------------------\n")
    tags.foreach{ case (key, value) => stringbuilder.append(key + "=>" + value.size.toString + "\n")}
    return stringbuilder.result()
  }
}

class TokenBase(){
  private val data = new mutable.HashMap[String, Token]()
  val tags = new concurrent.TrieMap[String, List[Token]]

  def add(t:Token):Unit = {
    data.update(t.id, t);
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
    val maybe = data.get(token_id)
    if( maybe.isDefined ){
      maybe.get.update(update_details)
    }
  }

  def remove(token_id:String):Unit = {
    var token = data.get(token_id);
    if( token.isDefined ){
      data.remove(token_id);
      token.get.extendedTags.foreach(s => {
        var maybe = tags.get(s)
        if (maybe.isDefined){
          tags.update(s, maybe.get.filterNot( item => item.id == token_id ))
        }
      });
    }
  }

  def get(token_id:String):Option[Token] = {data.get(token_id)}

  def toImmutable:ImmutableTokenBase = {
    return new ImmutableTokenBase(HashMap() ++ data, tags)
  }

  override def toString:String = {
    val stringbuilder = new mutable.StringBuilder()
    data.foreach{ case (key, value) => stringbuilder.append(key + " => " + value.name + " " + value.x + "," + value.y + "\n")}
    stringbuilder.append("-----------------------\n")
    tags.foreach{ case (key, value) => stringbuilder.append(key + "=>" + value.size.toString + "\n")}
    return stringbuilder.result()
  }
}

class ImmutableTokenIdGrid(val width:Int, val height:Int, val grid:ImmutableGrid[List[String]]) extends Gridlike[List[String]]{
  def get(x:Int, y:Int):List[String] = {
    return grid.get(x, y)
  }
}

class TokenIdGrid(val width:Int, val height:Int) extends Gridlike[List[String]] {
  private val data = new Grid[List[String]](width, height, ()=>List())

  def get(x:Int, y:Int):List[String] = {
    return data.get(x, y)
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



abstract class StateUpdate
case class MoveToken(val token_id:String, val x:Int, val y:Int) extends StateUpdate
case class CreateToken(val token_name:String, val x:Int, val y:Int) extends StateUpdate
case class DestroyToken(val token_id:String) extends StateUpdate
case class UpdateToken(val token_id:String, val update:String) extends StateUpdate
case class Message(val message:String, val x:Int, valy:Int) extends StateUpdate
case class GlobalMessage(val message:String) extends StateUpdate

class Snapshot(
  val width:Int,
  val height:Int,
  val terrainGrid:ImmutableTerrainGrid,
  val tokenGrid:ImmutableTokenIdGrid,
  val tokenBase:ImmutableTokenBase,
  val collisionGrid:ImmutableBitGrid){

  override def toString():String = {
    val symbolGrid = new Grid[String](width, height, ()=>"")
    terrainGrid.each( (x, y, terrain) => {
      symbolGrid.set(x, y, terrain.symbol)
    })
    tokenGrid.each( (x, y, stack) => {
      if( stack.length > 1){
        symbolGrid.set(x, y, stack.length.toString)
      }
      else if( stack.length == 1){
        val topStack = stack.last
        val maybe = tokenBase.get(topStack)
        if( maybe.isDefined){
          symbolGrid.set(x, y, maybe.get.symbol)
        }
      }
    })
    return tokenBase.toString + "\n+++++++++\n" + symbolGrid.toString
  }
}

class StateGrid(val width: Int, val height:Int, private val tokenCreator:(String, Int, Int) => Token, private val baseTerrain:Terrain){
  val terrainGrid = new TerrainGrid(width, height, baseTerrain)
  val tokenGrid = new TokenIdGrid(width, height)
  val tokenBase = new TokenBase()
  val collisionGrid = terrainGrid.getCollisions

  def toSnapshot():Snapshot = {
    return new Snapshot(width, height, terrainGrid.toImmutable, tokenGrid.toImmutable ,tokenBase.toImmutable, collisionGrid.toImmutable)
  }

  def stateUpdate(stateUpdate:StateUpdate):Boolean = stateUpdate match {
    case MoveToken(token_id, x, y) => {
      if(x >= width || x < 0 || y >= width || y < 0){
        return false
      }
      val maybe = tokenBase.get(token_id)
      if(maybe.isDefined)
      {
        val token = maybe.get
        val old_x = token.x
        val old_y = token.y
        if(token.isCollidable){
          if(!collisionGrid.set(x, y)){
            // I've set a bit that's already set
            return false
          }
          else{
            tokenGrid.set(x, y, token.id)
            tokenGrid.remove(old_x, old_y, token.id)
            collisionGrid.unset(old_x, old_y)
            token.x = x
            token.y = y
            return true
          }
        }
        else{
          tokenGrid.set(x, y, token.id)
          return true
        }
      }
      return true;
    }
    case CreateToken(token_name, x, y) => {
      val token = tokenCreator(token_name, x, y)
      if(token.isCollidable){
        if(!collisionGrid.set(x, y)){
          // I've set a bit that's already set
          println("Can't create token " + token_name)
          return false
        }
        else{
          println("Creating token " + token_name + "-" + token.id + "at " + x + "," + y)
          tokenBase.add(token)
          tokenGrid.set(x, y, token.id)
          return true
        }
      }
      else{
        println("Creating token " + token_name + "-" + token.id + "at " + x + "," + y)
        tokenBase.add(token)
        tokenGrid.set(x, y, token.id)
        return true
      }
    }
    case DestroyToken(token_id) => {
      val maybe = tokenBase.get(token_id)
      if(maybe.isDefined){
        var token = maybe.get
        tokenGrid.remove(token.x, token.y, token.id)
        tokenBase.remove(token_id);
      }
      return true;
    }
    case UpdateToken(token_id, update_details)=>{ tokenBase.update(token_id, update_details); return true;}
    case Message(message, x, y) => { println(message); return true; }
    case GlobalMessage(message) => { println(message); return true;}
  }
}


case class Fence() extends Terrain{
  override def isCollidable = true
  override def symbol = "F"
}

case class Dirt() extends Terrain {
}

class UnknownToken(sx:Int, sy:Int) extends Token(sx,sy){}

class Grass(sx:Int, sy:Int) extends Token(sx,sy){
  override def name = "grass"
  override protected def tags = Vector("grass")
  override def symbol = "G"
}

class Sheep(sx:Int, sy:Int) extends Token(sx,sy){
  val random = new scala.util.Random(System.nanoTime())
  var grass_eaten = 0;
  override val name = "sheep"
  override protected def tags = Vector("sheep")
  override def isTickable = true
  override def isCollidable = true
  override def hasState = true
  override def symbol = "S"
  override def collision():Unit = {
  }
  override def tick(state:Snapshot):List[StateUpdate] = {
    // If standing on grass, eat grass.
    val tokens_where_i_am_standing = state.tokenBase.convertTokenIds(state.tokenGrid.getStack(this))
    val grass_where_i_am_standing = tokens_where_i_am_standing.filter( (token) => token.name == "grass")

    if (grass_where_i_am_standing.length > 0){
      println("I am standing on " + grass_where_i_am_standing.length + " grasses")
      return List(
        DestroyToken(grass_where_i_am_standing.head.id),
        Message("MONCH MONCH", x,y),
        UpdateToken(this.id, "INCREMENT_GRASS"))
    }

    if(grass_eaten > 0 && grass_where_i_am_standing.length == 0 && random.nextBoolean()){
       return List(
         CreateToken("poop", x, y),
         Message("PTHBBBT", x, y),
         UpdateToken(this.id, "DECREMENT_GRASS"))
    }

    // Move Randomly, Up, Down, Left, or Right
    if( random.nextBoolean() ){
      if( random.nextBoolean()){
        return List(MoveToken(this.id, x+1, y))
      }
      else{
        return List(MoveToken(this.id, x-1, y))
      }
    }
    else{
      if( random.nextBoolean()){
        return List(MoveToken(this.id, x, y+1))
      }
      else{
        return List(MoveToken(this.id, x, y-1))
      }
    }
  }
  override def update(update_details:String):Unit = {
    if(update_details == "INCREMENT_GRASS"){
      grass_eaten = grass_eaten + 1
      println("Incrementing grass: " + grass_eaten)
    }
    if(update_details == "DECREMENT_GRASS"){
      grass_eaten = grass_eaten - 1
      println("Decrementing grass: " + grass_eaten)
    }
  }
}

class Poop(sx:Int, sy:Int) extends Token(sx,sy) {
  var timer = 5;
  override def name = "poop"
  override protected def tags = Vector("poop")
  override def isTickable = true
  override def isCollidable = false
  override def hasState = true
  override def symbol = "P"
  override def tick(state:Snapshot):List[StateUpdate] = {
    if( timer > 0){
      return List(UpdateToken(this.id, "DECREMENT_POOP_COUNTER"))
    }
    else{

      return List(
        DestroyToken(this.id),
        Message("SPROUT!", x, y),
        CreateToken("grass", x, y)
      )

    }

  }
  override def update(details:String){
    if(details == "DECREMENT_POOP_COUNTER"){
      timer -= 1
    }
  }
}

object Game{
  def createToken(name:String, x:Int, y:Int):Token = {
    if(name == "grass"){
      return new Grass(x, y)
    }
    if(name == "sheep"){
      return new Sheep(x, y)
    }
    if(name == "poop"){
      return new Poop(x, y)
    }
    return new UnknownToken(x, y)
  }
}
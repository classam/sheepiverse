

case class Fence() extends Terrain{
  override def isCollidable = true
  override def symbol = "F"
}

case class Dirt() extends Terrain {
}

class UnknownToken(x:Int, y:Int) extends Token(x,y){}

class Grass(x:Int, y:Int) extends Token(x,y){
  override def name = "grass"
  override protected def tags = Vector("grass")
  override def symbol = "G"
}

class Sheep(x:Int, y:Int) extends Token(x,y){
  val random = new scala.util.Random(System.nanoTime())
  var grass_eaten = 0;
  override val name = "sheep"
  override protected def tags = Vector("sheep")
  override def isTickable = true
  override def isCollidable = true
  override def hasState = true
  override def symbol = "S"
  override def collision():Unit = {
    println("BAA!")
  }
  override def tick(state:Snapshot):List[StateUpdate] = {
    println("baa")
    // If standing on grass, eat grass.
    val grass_where_i_am_standing = state.tokenGrid.grid.get(x, y).filter((token_id) =>{ state.tokenBase.map(token_id).name == "grass"})

    if (grass_where_i_am_standing.length > 0){
      println("I am standing on " + grass_where_i_am_standing.length + " grasses")
      return List(
        DestroyToken(grass_where_i_am_standing.head),
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
        return List(MoveToken(this.id, x, y+0))
      }
    }
  }
  override def update(update_details:String):Unit = {
    println("update")
    if(update_details == "INCREMENT_GRASS"){
      grass_eaten += 2
    }
    if(update_details == "DECREMENT_GRASS"){
      grass_eaten -= 1
    }
  }
}

class Poop(x:Int, y:Int) extends Token(x,y) {
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
        DestroyToken(this.id)
      )
      /*
      return List(
        DestroyToken(this.id),
        Message("SPROUT!", x, y),
        CreateToken("grass", x, y)
      )
      */
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
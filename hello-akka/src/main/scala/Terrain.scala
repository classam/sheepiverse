

abstract class Terrain() {
  def isCollidable:Boolean = false
  def symbol:String = "."
}

class ImmutableTerrainGrid(val width:Int, val height:Int, val terrainGrid:ImmutableGrid[Terrain], val collisionGrid:ImmutableBitGrid) extends Gridlike[Terrain]{
  def get(x:Int, y:Int):Terrain = { terrainGrid.get(x, y) }
}

class TerrainGrid(val width: Int, val height:Int, private val baseTerrain:Terrain) extends Gridlike[Terrain]{
  require(!baseTerrain.isCollidable)
  private val grid:Grid[Terrain] = new Grid(width, height, ()=>baseTerrain);
  private val collisionGrid = new BitGrid(width, height);

  def get(x:Int, y:Int):Terrain = { grid.get(x, y) }

  def set(x:Int, y:Int, terrain:Terrain):Unit = {
    if(terrain.isCollidable){
      collisionGrid.set(x, y)
    }
    grid.set(x, y, terrain)
  }
  def getCollisions:BitGrid = { collisionGrid.copy }

  override def toString:String = {grid.toString}
  def toImmutable:ImmutableTerrainGrid = {
    return new ImmutableTerrainGrid(width, height, grid.toImmutable(), collisionGrid.toImmutable)
  }
}
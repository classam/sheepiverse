import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag

trait Gridlike[T]{
  def height:Int
  def width:Int
  def get(x:Int, y:Int):T

  def each(fn:(Int, Int, T)=>Any):Unit = {
    for( i <- 0 until width){
      for( j <- 0 until height){
        fn(i, j, get(i, j))
      }
    }
  }

  def each(fn:(T)=>Any):Unit = {
    each((x:Int, y:Int, t:T)=>fn(t))
  }

  def filter(fn:(T)=>Boolean):immutable.Vector[T] = {
    val builder = Vector.newBuilder[T]
    each((t:T) => if(fn(t)){builder.+=(t)})
    return builder.result()
  }
}

class ImmutableGrid[T:ClassTag](val width:Int, val height:Int, private val data:immutable.Vector[immutable.Vector[T]]) extends Gridlike[T]{
  def get(x:Int, y:Int):T = data(y)(x)
}

class Grid[T:ClassTag](val width:Int, val height:Int, private val data:Array[Array[T]]) extends Gridlike[T]{
  require(width > 0)
  require(height > 0)

  def this(width:Int, height:Int, fillFn:()=>T) = this(width, height, Array.fill[T](height, width){fillFn()} )

  def set(x:Int, y:Int, obj:T):Unit = data(y)(x) = obj
  def get(x:Int, y:Int):T = data(y)(x)
  override def toString:String = data.map(x => x.mkString("")).mkString("\n")

  def toImmutable():ImmutableGrid[T] = {
    val builder = Vector.newBuilder[Vector[T]]
    data.foreach(i => {
      val innerbuilder = Vector.newBuilder[T]
      i.foreach(j => innerbuilder.+=(j))
      builder.+=(innerbuilder.result())
    })
    return new ImmutableGrid(width, height, builder.result())
  }
}

class ImmutableBitGrid(val width:Int, val height:Int, private val data:immutable.Vector[immutable.BitSet]) extends Gridlike[Boolean]{
  def get(x:Int, y:Int):Boolean = {
    require(x < width && y < height)
    return data(y).contains(x)
  }

  def or(b:ImmutableBitGrid):ImmutableBitGrid = {
    require(width == b.width && height == b.height)
    val builder = Vector.newBuilder[immutable.BitSet]
    for(i <- 0 until data.length){
       builder.+=(data(i) | b.data(i))
    }
    return new ImmutableBitGrid(width, height, builder.result())
  }
}

class BitGrid(val width:Int, val height:Int, private val data:Array[mutable.BitSet]) extends Gridlike[Boolean]{
  require(width > 0 && height > 0)

  def this(width:Int, height:Int) = this(width, height, Array.fill[mutable.BitSet](height){new mutable.BitSet(width)})

  private def long_to_string(x:Long):String = {
    String.format("%64s", x.toBinaryString).replace(' ', '0')
  }

  def set(x:Int, y:Int):Boolean = {
    require(x < width && y < height)
    return data(y).add(x)
  }
  def unset(x:Int, y:Int):Boolean = {
    require(x < width && y < height)
    return data(y).remove(x)
  }
  def get(x:Int, y:Int):Boolean = {
    require(x < width && y < height)
    return data(y).contains(x)
  }

  def or(b:BitGrid):Unit = {
    require(width == b.width && height == b.height)
    for(i <- 0 until data.length){
      data(i) = data(i) | b.data(i)
    }
  }

  def copy:BitGrid = {
    val tempArray = new Array[mutable.BitSet](height)
    data.zipWithIndex.foreach{ case(bitset,i) =>
      tempArray(i) = bitset.clone
    }
    return new BitGrid(width, height, tempArray)
  }

  def toImmutable:ImmutableBitGrid = {
    val builder = Vector.newBuilder[immutable.BitSet]
    data.foreach(i => {
      builder.+=(i.toImmutable)
    })
    return new ImmutableBitGrid(width, height, builder.result())
  }

  override def toString:String = {
    data.map(bitset => bitset.toBitMask.map(word => long_to_string(word).reverse).mkString("").substring(0, width)).mkString("\n")
  }
}
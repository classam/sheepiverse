import org.scalatest._
import org.scalatest.matchers._

class GridSpec extends FlatSpec with ShouldMatchers {

  "A Grid" should "be filled with the fillTile" in {
    val grid = new Grid[Int](5,5,()=>3)
    grid.get(2,2) should be(3)
  }

  it should "create a grid of the correct width and height" in {
    val grid = new Grid[String](2,5,()=>"P")
    grid.toString() should be ("PP\nPP\nPP\nPP\nPP")
  }

  it should "treat the origin as the top-left" in {
    val grid = new Grid[String](3,3,()=>"P")
    grid.set(0,0,"S")
    grid.toString() should be ("SPP\nPPP\nPPP")
  }

  it should "have the x-axis be the first variable" in {
    val grid = new Grid[String](3,5,()=>"P")
    grid.set(0,2,"S")
    grid.set(0,4,"S")
    grid.toString() should be ("PPP\nPPP\nSPP\nPPP\nSPP")
  }

  it should "let me filter out all but one value" in {
    val grid = new Grid[String](3,3,()=>"P")
    grid.set(0,2,"S")
    grid.set(1,1,"S")
    grid.filter((t:String) => t == "S").length should be (2)
    grid.filter((t:String) => t == "P") should be (Vector("P","P","P","P","P","P","P"))
  }

  it should "have the y-axis be the second variable" in {
    val grid = new Grid[String](3,5,()=>"P")
    grid.set(1,0,"S")
    grid.set(1,2,"S")
    grid.toString() should be ("PSP\nPPP\nPSP\nPPP\nPPP")
  }

  it should "fail to create a grid of invalid proportions" in {
    intercept[IllegalArgumentException]{
      val grid = new Grid[String](-3, 4, ()=>"P")
    }
  }

  it should "fail if I set an item out of bounds" in {
    intercept[ArrayIndexOutOfBoundsException]{
      val grid = new Grid[String](3,5,()=>"P")
      grid.set(100,1000,"S")
    }
  }

  "A BitGrid" should "be filled with 0s" in {
    val grid = new BitGrid(64, 1)
    grid.toString.length should be (64)
    grid.toString should be ("0000000000000000000000000000000000000000000000000000000000000000")
  }

  it should "have 1 bit if the width is 1" in {
    val grid = new BitGrid(1, 1)
    grid.toString.length should be (1)
    grid.toString should be ("0")
  }

  it should "have 100 bits if the width is 100" in {
    val grid = new BitGrid(100, 1)
    grid.toString.length should be (100)
  }

  it should "allow me to set the first bit" in {
    val grid = new BitGrid(64, 1)
    grid.set(0, 0)
    grid.toString should be ("1000000000000000000000000000000000000000000000000000000000000000")
  }

  it should "allow me to set the last bit" in {
    val grid = new BitGrid(64, 1)
    grid.set(63, 0)
    grid.toString should be ("0000000000000000000000000000000000000000000000000000000000000001")
  }

  it should "allow me to set the 99th bit" in {
    val grid = new BitGrid(100, 1)
    grid.set(99, 0)
    grid.toString should be ("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001")
  }

  it should "allow me to unset a bit" in {
    val grid = new BitGrid(64, 1)
    grid.set(0, 0)
    grid.unset(0, 0)
    grid.toString should be ("0000000000000000000000000000000000000000000000000000000000000000")
  }

  it should "not allow me to set a bit that's a little bit out of width range" in {
    intercept[IllegalArgumentException]{
      val grid = new BitGrid(100, 1)
      grid.set(101, 0)
    }
  }

  it should "not allow me to set a bit that's way out of width range" in {
    intercept[IllegalArgumentException] {
      val grid = new BitGrid(100, 1)
      grid.set(110, 0)
    }
  }

  it should "not allow me to set a bit that's way out of height range" in {
    intercept[IllegalArgumentException] {
      val grid = new BitGrid(100, 100)
      grid.set(1, 1000)
    }
  }

  it should "not have a bit where one has not been set" in {
    val grid = new BitGrid(50,50)
    grid.get(3, 7) should not be(true)
  }

  it should "return true when I set a bit" in {
    val grid = new BitGrid(50,50)
    grid.set(3, 7) should be(true)
  }
  it should "have a bit where one has been set" in {
    val grid = new BitGrid(50,50)
    grid.get(3, 7) should not be(true)
    grid.set(3, 7)
    grid.get(3, 7) should be(true)
  }
  it should "return false when I set a bit that has already been set" in {
    val grid = new BitGrid(50,50)
    grid.set(3, 7)
    grid.set(3, 7) should not be(true)
  }

  it should "have the y-axis be the second variable" in {
    val grid = new BitGrid(3,3)
    grid.set(2, 2)
    grid.toString should be ("000\n000\n001")
  }

  it should "have identical data in the copy" in {
    val grid = new BitGrid(5,5)
    grid.set(2,2)
    val newgrid = grid.copy
    grid.set(3,3)
    newgrid.get(2,2) should be (true)
    newgrid.get(3,3) should not be (true)
  }

  it should "be able to count the ones" in {
    val grid = new BitGrid(5,5)
    grid.set(1,1)
    grid.set(2,2)
    grid.set(3,3)
    grid.set(4,4)
    grid.filter(x=>x).length should be (4)
  }

  it should "be able to combine two bitgrids" in {
    val grid = new BitGrid(5,5)
    grid.set(1,1)
    grid.set(2,2)
    val secondgrid = new BitGrid(5,5)
    secondgrid.set(1,1)
    secondgrid.set(3,3)
    grid.or(secondgrid)
    grid.get(1,1) should be (true)
    grid.get(2,2) should be (true)
    grid.get(3,3) should be (true)
    grid.get(4,4) should not be (true)
  }

}

// Thomas Hood - Lab for college, generate a picture object that has 3 possible shapes per row and is subject to some rules
// Notes
// ::RS:: == Row Starts
// ::RE:: == Row Ends

abstract class Picture{
  def area(): Double
}

case class Picture1(x: Row) extends Picture {
  override def toString = x.toString
  def area() = x.area()
}

case class Picture2(x: Row, y: Row) extends Picture {
  def area() = x.area() + y.area()
  override def toString = {
    x.toString + y.toString
  }
}

case class Picture3(x: Row, y: Row, z: Row) extends Picture {
  def area() = x.area() + y.area() + z.area()
  override def toString = {
    x.toString + y.toString + z.toString
  }
}

abstract class Row extends Picture{
  def area():Double
}

case class EmptyRow() extends Row {
  def area() = 0
  override def toString = "|::RS::([empty])::RE::|"
}

case class Row1(x: Shape) extends Row {
  def area() = x.area()
  override def toString = {
    "|::RS::([" + this.x.toString + "])::RE::|"
  }
}

case class Row2(x: Shape, y: Shape) extends Row {
  def area() = x.area() + y.area()
  override def toString = {
    "|::RS::([ " + this.x.toString + " ]), " + "([ " + this.y.toString + " )]::RE::|"
  }
}

case class Row3(x: Shape, y: Shape, z: Shape) extends Row {
  def area() = x.area() + y.area() + z.area()
  override def toString = {
    "|::RS::([ " + this.x.toString + " ]), " + "([ " + this.y.toString + " ]), " + " ([ " + this.z.toString + " ])::RE::|"
  }
}

abstract class Shape(c: String) {
  def name(): String
  def area(): Double
  def color() = c match {
    case "red" => "(R)"
    case "blue" => "(B)"
    case "green" => "(G)"
  }
  override def toString = name() + " " + color() + " " + "[" + area() + "]"
}

// Triangle class
case class Triangle(a: Int, b: Int, c: String) extends Shape(c) {
  def this(d:String, e: Int, f:Int) = this(e,f,d)
  def name() = "T"
  def area() = a * b / 2
}

// Square class - Removed b parameter because technically a square should have 4 equal sides
case class Square(a: Int, c: String) extends Shape(c) {
  def this(d:String, e: Int) = this(e, d)
  def name() = "S"
  def area() = a * a
}

// Circle class
case class Circle(p: Int, c: String) extends Shape(c) {
  def this(d:String, e: Int) = this(e, d)
  def name() = "C"
  def area() = (p * p) * Math.PI
}

// Just added these for the examples
def Red() = "red"
def Green() = "green"
def Blue() = "blue"

// Testing
val s = new Picture1(new Row1(new Triangle(5,5,"red")))
s.area()
val t = Picture1(new Row1(new Triangle(Red(), 5, 5)))
t.area()

val x = Picture2(new Row3(new Square(Red(), 1), new Circle(Green(), 1), new Triangle(Green(), 1, 2)), Row2(new Square(Red(), 1), new Square(Red(), 1)))

val y = transform(x)
// Result should match up with PDF(except the double)

// -- End Testing

// Transform function
def transform(p: Picture): Picture = p match{
  case Picture1(x) =>
    Picture1(transform1(x))
  case Picture2(x,y) =>
    Picture2(transform1(x),transform1(y))
  case Picture3(x,y,z) =>
    Picture3(transform1(x),transform1(y),transform1(z))
}

def transform1(r: Row): Row = r match {
  // Square - Two elements in a row
  case Row2(Square(a, c), Square(b, d)) =>
    // Add square sides together, no colour specific rules described
    val newside = a + b
    new Row1(new Square(newside, c))

  // Triangle - Two elements in a row
  case Row2(Triangle(a, b, c), Triangle(d, e, f)) =>
    // Add bases and heights together, no colour specific rules
    val newbase = a + d
    val newheight = b + e
    new Row1(new Triangle(newbase, newheight, c))

  // Circle - Two elements in a row, no colour specific rules
  case Row2(Circle(a, b), Circle(c, d)) =>
    // Add radius together
    val newradius = a + c
    new Row1(new Circle(newradius, b))

  // If all shapes are different, shapes should get * 2 bigger

  /*
  Possible arrangements; Goes without saying, if bigger possible arrangements write a loop to calc that
  S, C, T
  S, T, C
  C, T, S
  C, S, T
  T, S, C
  T, C, S -- 6 Total
   */

  case Row3(Square(a, b), Circle(c, d), Triangle(e, f, g)) =>
    Row3(Square(a * 2, b), Circle(c * 2, d), Triangle(e * 2, f * 2, g))

  case Row3(Square(a, b), Triangle(c, d, e), Circle(f, g)) =>
    Row3(Square(a * 2, b), Triangle(c * 2, d * 2, e), Circle(f * 2, g))

  case Row3(Circle(a, b), Triangle(c, d, e), Square(f, g)) =>
    Row3(Circle(a * 2, b), Triangle(c * 2, d * 2, e), Square(f * 2, g))

  case Row3(Circle(a, b), Square(c, d), Triangle(e, f, g)) =>
    Row3(Circle(a * 2, b), Square(c * 2, d), Triangle(e * 2, f * 2, g))

  case Row3(Triangle(a, b, c), Square(d, e), Circle(f, g)) =>
    Row3(Triangle(a * 2, b * 2, c), Square(d * 2, e), Circle(f * 2, g))

  case Row3(Triangle(a, b, c), Circle(d, e), Square(f, g)) =>
    Row3(Triangle(a * 2, b * 2, c), Circle(d * 2, e), Square(f * 2, g))


  // If there are 3 shapes in a row, and the first and last are the same type, swap them

  /*
  Possible arrangements;
  S,C,S
  S,S,S
  S,T,S
  */

  case Row3(Square(a,b), Circle(c,d), Square(e,f)) =>
    Row3(Square(e,f), Circle(c,d), Square(a,b))

  case Row3(Square(a,b), Square(c,d), Square(e,f)) =>
    Row3(Square(e,f), Square(c,d), Square(a,b))

  case Row3(Square(a,b), Triangle(c,d,e), Square(f,g)) =>
    Row3(Square(f,g), Triangle(c,d,e), Square(a,b))

  /*
C,S,C
C,T,C
C,C,C
*/

  case Row3(Circle(a,b), Square(c,d), Circle(e,f)) =>
    Row3(Circle(e,f), Square(c,d), Circle(a,b))

  case Row3(Circle(a,b), Triangle(c,d,e), Circle(f,g)) =>
    Row3(Circle(f,g), Triangle(c,d,e), Circle(a,b))

  case Row3(Circle(a,b), Circle(c,d), Circle(e,f)) =>
    Row3(Circle(e,f), Circle(c,d), Circle(a,b))

  /*
T,S,T
T,C,T
T,T,T -- 9 total
 */

  case Row3(Triangle(a,b,c), Square(d,e), Triangle(f,g,h)) =>
    Row3(Triangle(f,g,h), Square(d,e), Triangle(a,b,c))

  case Row3(Triangle(a,b,c), Circle(d,e), Triangle(f,g,h)) =>
    Row3(Triangle(f,g,h), Circle(d,e), Triangle(a,b,c))

  case Row3(Triangle(a,b,c), Triangle(d,e,f), Triangle(g,h,i)) =>
    Row3(Triangle(g,h,i), Triangle(d,e,f), Triangle(a,b,c))

  //if there are three shapes in a row, and all are of a different color, this line should cleared
  case Row3(x,y,z) =>
    val color1 = x.color()
    val color2 = y.color()
    val color3 = z.color()
    if(color1 != color2 && color2 != color3 && color3 != color1)
      EmptyRow()
    else
      Row3(x,y,z)
}

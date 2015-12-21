// Thomas Hood -- Stack Data Structure for Ints
// Could easily be converted to Strings or other data type -- just swap the types

// Lists
var newlist = List(0,1,2)
var newlist2 = List(3,4,5)
var newlist3 = List(6,7,8)

class stack(x: List[Int]) {
  val stack: scala.collection.mutable.Stack[List[Int]] = new scala.collection.mutable.Stack[List[Int]]
  var dataSize: Int=1
  // Must be a List of Strings -- Precondition for creation of data structure
  //require(x==true)

  // Takes List of Strings as first data to be pushed to stack
  val first: List[Int] = x
  push(x)

  // Check that we have the data structure created -- Postcondition
  assert(stack.isEmpty!=true)

  def push(t: List[Int])={
    // Precondition -- must have List to push and List must be > 0
    require(dataSize < 5 && t.length > 0)
    stack.push(t)
  }
  def pop()={
    // Precondition -- stack is not empty
    require(dataSize > 0)
    stack.pop()
  }
  def size()={
    stack.length.ensuring(dataSize => dataSize >= 0)
  }
  def peek()={
    // Precondition -- stack is not empty
    require(stack.isEmpty!=true && dataSize > 0)
    stack.top
  }
  def isEmpty() = {
    // Stack must be able to return a size
    require(stack.hasDefiniteSize==true)
    if(stack.isEmpty==true){
      "Empty Stack"
    }
    else{
      "Stack: " + stack.mkString
    }
  }
}

// Testing
var newstack = new stack(newlist)
newstack.push(newlist2)
newstack.push(newlist3)
newstack.pop()
newstack.pop()
newstack.isEmpty()
newstack.pop()
newstack.isEmpty()
// Preconditions -- done
// Postconditions -- assertion done
// ensuring -- done

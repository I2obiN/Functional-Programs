def memetriangle(str: String, n: Int): String = {
  var nl = "\n";
  var i=1;

  for(i <- 1 to n){
    nl += repeat(" ", n-i) + repeat(str, i*2-1).substring(0, i*2-1)+ "\n";
  }

  def repeat(str: String, x: Int): String = {
    var y="";
    for(i <- 1 to x){ y+=str; }
    return y;
    }
  return nl;
}

memetriangle("MEME TRIANGLE", 30)

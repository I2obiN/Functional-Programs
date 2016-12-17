def piGet(n: Int) = {
  quickPi(0, 1, n, 0, 1)
}

def quickPi(sum: Double, a: Int, n: Int, count: Int, sign: Int) {
  if(count < n){
    System.out.println(sum*4)
    quickPi(sum + (piCalc(a)*sign), a+2, n, count+1, sign*(-1))
  }
}

def piCalc(a: Int) = { 1.0 / a }

val start = System.currentTimeMillis()
piGet(400000)
val end = System.currentTimeMillis()
val timeSecs = (end - start) / 1000.00

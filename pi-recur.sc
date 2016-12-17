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

System.out.println(piGet(10000))

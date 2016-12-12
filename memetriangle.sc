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

/* output:
                             M
                            MEM
                           MEME 
                          MEME TR
                         MEME TRIA
                        MEME TRIANG
                       MEME TRIANGLE
                      MEME TRIANGLEME
                     MEME TRIANGLEMEME
                    MEME TRIANGLEMEME T
                   MEME TRIANGLEMEME TRI
                  MEME TRIANGLEMEME TRIAN
                 MEME TRIANGLEMEME TRIANGL
                MEME TRIANGLEMEME TRIANGLEM
               MEME TRIANGLEMEME TRIANGLEMEM
              MEME TRIANGLEMEME TRIANGLEMEME 
             MEME TRIANGLEMEME TRIANGLEMEME TR
            MEME TRIANGLEMEME TRIANGLEMEME TRIA
           MEME TRIANGLEMEME TRIANGLEMEME TRIANG
          MEME TRIANGLEMEME TRIANGLEMEME TRIANGLE
         MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEME
        MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME
       MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME T
      MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME TRI
     MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME TRIAN
    MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME TRIANGL
   MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME TRIANGLEM
  MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEM
 MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME 
MEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME TRIANGLEMEME TR
*/

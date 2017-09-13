import java.util.Random
//Matthew Wong
//CSE 262
//Puzzle Solver


object PuzzleSolver {
  //main method that runs everything
  def main(args: Array[String])={
    //the total that is required for each row
    val total = 38

    //An array of all the numbers that will be added to the hexagon
    var numbers = Array(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)

    //an array of numbers that have been used
    var used = Array[Int](19)

    //all the variables for the numbers in the hexagon
    val a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s: Int =0

    //row from top to bottom
    var row1= Array(a,b,c)
    var row2=Array(d,e,f,g)
    var row3=Array(h,i,j,k,l)
    var row4=Array(m,n,o,p)
    var row5=Array(q,r,s)

    //row from left to right
    var r6=Array(a,d,h)
    var r7=Array(b,e,i,m)
    var r8=Array(c,f,j,n,q)
    var r9=Array(g,k,o,r)
    var r10=Array(l,p,s)

    //rows from right to left
    var r11=Array(c,g,l)
    var r12=Array(b,f,k,p)
    var r13=Array(a,e,j,o,s)
    var r14=Array(d,i,n,r)
    var r15=Array(h,m,q)

    //creates the hexagon
    var hexagon = Array(row1,row2,row3,row4,row5)
    var hexagon2 = Array(r6,r7,r8,r9,r10)
    var hexagon3 = Array(r11,r12,r13,r14,r15)

    var counter = 0

    //prints out the first hexagon
    var outerloop = 0
    var innerloop = 0
    while(outerloop<hexagon.length){
      while(innerloop<hexagon(outerloop).length){
        hexagon(outerloop)(innerloop) = numbers(r)
        print(numbers(counter) + " ")
        //counter = counter+1
        innerloop=innerloop+1
      }
      innerloop = 0
      outerloop=outerloop+1
      println(" ")
    }
  }

  def sum
}

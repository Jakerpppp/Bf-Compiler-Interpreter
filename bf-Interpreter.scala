// Main Part 5 about an Interpreter for 
// the Brainf*** language
//==============================================


object M5a {

// representation of BF memory 

type Mem = Map[Int, Int]

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================

// (1)
def load_bff(name: String) : String = {
    Try(Source.fromFile(name)("ISO-8859-1").mkString).getOrElse("")
}

// (2) 

def sread(mem: Mem, mp: Int) : Int = {
    mem.getOrElse(mp,0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
    mem + (mp -> v)
}

// (3) 
def jumpRight(prog: String, pc: Int, level: Int) : Int = prog match{
    case s if pc == prog.length => pc
    case s if prog(pc) == ']' && level == 0 => pc + 1
    case s if prog(pc) == ']' && level != 0 => jumpRight(prog, pc + 1, level - 1)
    case s if prog(pc) == '[' => jumpRight(prog, pc + 1, level + 1)
    case s => jumpRight(prog, pc + 1, level)
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = prog match {
    case s if pc == -1 => pc
    case s if prog(pc) == '[' && level == 0 => pc + 1
    case s if prog(pc) == '[' && level != 0 => jumpLeft(prog, pc - 1, level - 1)
    case s if prog(pc) == ']' => jumpLeft(prog, pc - 1, level + 1)
    case s => jumpLeft(prog, pc - 1, level)
}


// testcases
//jumpRight("""--[..+>--],>,++""", 3, 0)         // => 10
//jumpLeft("""--[..+>--],>,++""", 8, 0)          // => 3
//jumpRight("""--[..[+>]--],>,++""", 3, 0)       // => 12
//jumpRight("""--[..[[-]+>[.]]--],>,++""", 3, 0) // => 18
//jumpRight("""--[..[[-]+>[.]]--,>,++""", 3, 0)  // => 22 (outside)
//jumpLeft("""[******]***""", 7, 0)              // => -1 (outside)



// (4) 

def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = prog match {
    case s if pc == prog.length || pc == -1 => mem
    case s if prog(pc) == '>' => compute(prog, pc + 1, mp + 1, mem)
    case s if prog(pc) == '<' => compute(prog, pc + 1, mp - 1, mem)
    case s if prog(pc) == '+' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem,mp) + 1))
    case s if prog(pc) == '-' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem,mp) - 1))
    case s if prog(pc) == '.' => {
        print(sread(mem,mp).toChar)
        compute(prog, pc + 1, mp, mem)
    }
    case s if prog(pc) == '[' && sread(mem,mp) == 0 => compute(prog, jumpRight(prog,pc + 1, 0), mp, mem)
    case s if prog(pc) == '[' && sread(mem,mp) != 0 => compute(prog, pc + 1, mp, mem)
    case s if prog(pc) == ']' && sread(mem,mp) != 0 => compute(prog, jumpLeft(prog, pc - 1, 0), mp, mem)
    case s if prog(pc) == ']' && sread(mem,mp) == 0 => compute(prog, pc + 1, mp, mem)
    case _ => compute(prog,pc + 1,mp,mem)
}

def run(prog: String, m: Mem = Map()) = {
    compute(prog, 0, 0, m)
}

// (5)
def generate(msg: List[Char]) : String = msg match{
    case Nil => ""
    case hd::tl => ("+" * hd.toInt) + ".[-]" + generate(tl)
}



// some sample bf-programs collected from the Internet
//=====================================================


// some contrived (small) programs
//---------------------------------

// clears the 0-cell
//run("[-]", Map(0 -> 100))    // Map will be 0 -> 0

// moves content of the 0-cell to 1-cell
//run("[->+<]", Map(0 -> 10))  // Map will be 0 -> 0, 1 -> 10


// copies content of the 0-cell to 2-cell and 4-cell
//run("[>>+>>+<<<<-]", Map(0 -> 42))    // Map(0 -> 0, 2 -> 42, 4 -> 42)


// prints out numbers 0 to 9
//run("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")


// some more "useful" programs
// -----------------------------

// hello world program 1
// run("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
//       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""")

// hello world program 2
// run("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
//       +.<<+++++++++++++++.>.+++.------.--------.>+.>.""")

// hello world program 3
// run("""+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..
//       +++.>-.------------.<++++++++.--------.+++.------.--------.>+.""")

 
// draws the Sierpinski triangle
//run(load_bff("sierpinski.bf"))


//fibonacci numbers below 100
// run("""+++++++++++
//      >+>>>>++++++++++++++++++++++++++++++++++++++++++++
//      >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
//      +<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
//      <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
//      -]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
//      >[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
//      +++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
//      ++++++++++++++++++++++++++++++++++++++++++++.[-]<<
//      <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
//      [-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]""")

//outputs the square numbers up to 10000
// run("""++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
//       >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
//       <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""")


// calculates 2 to the power of 6 
//(example from a C-to-BF compiler at https://github.com/elikaski/BF-it)
// run(""">>[-]>[-]++>[-]++++++><<<>>>>[-]+><>[-]<<[-]>[>+<<+>-]>[<+>-]
//       <><[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]
//       <[>+>+<<-]>[<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<[[-]>[-]<<[>+>
//       +<<-]>>[<<+>>-][-]>[-]<<<<<[>>>>+>+<<<<<-]>>>>>[<<<<<+>>>>>-]
//       <<>>[-]>[-]<<<[>>>+<<<-]>>>[<<[<+>>+<-]>[<+>-]>-]<<<>[-]<<[-]
//       >[>+<<+>-]>[<+>-]<><[-]>[-]<<<[>>+>+<<<-]>>>-[<<<+>>>-]<[-]>[-]
//       <<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]<[>+>+<<-]>
//       [<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<][-]>[-]<<[>+>+<<-]>>[<<+>
//       >-]<<<<<[-]>>>>[<<<<+>>>>-]<<<<><>[-]<<[-]>[>+<<+>-]>[<+>-]<>
//       <[-]>[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-]<<>>[-]>[-]>[-]>[-]>[-]>
//       [-]>[-]>[-]>[-]>[-]<<<<<<<<<<>>++++++++++<<[->+>-[>+>>]>[+[-<+
//       >]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<
//       ]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++
//       ++++<]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<<><<<""")



// a Mandelbrot set generator in brainf*** written by Erik Bosman
// (http://esoteric.sange.fi/brainfuck/utils/mandelbrot/)
//
//run(load_bff("mandelbrot.bf"))


// a benchmark program (counts down from 'Z' to 'A')
//
//run(load_bff("benchmark.bf"))

// calculates the Collatz series for numbers from 1 to 30
//
//run(load_bff("collatz.bf"))


}

// Main Part 5 about a "Compiler" for the Brainf*** language
//============================================================


object M5b {

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================

def load_bff(name: String) : String = {
    Try(Source.fromFile(name)("ISO-8859-1").mkString).getOrElse("")
}

def sread(mem: Mem, mp: Int) : Int = {
    mem.getOrElse(mp,0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
    mem + (mp -> v)
}

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

def generate(msg: List[Char]) : String = msg match{
    case Nil => ""
    case hd::tl => ("+" * hd.toInt) + ".[-]" + generate(tl)
}


// (6) 
def jtable(pg: String): Map[Int, Int] = {
  val x = for (n <- (0 to pg.length - 1).toList) yield {
    if (pg(n) == '[') (n -> jumpRight(pg, n + 1, 0))
    else if (pg(n) == ']') (n -> jumpLeft(pg, n - 1, 0))
    else (n -> -1)
  }
  x.toMap.filter(_._2 != -1)
}

// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = pg match {
    case s if pc == pg.length || pc == -1 => mem
    case s if pg(pc) == '>' => compute2(pg, tb,pc + 1, mp + 1, mem)
    case s if pg(pc) == '<' => compute2(pg, tb,pc + 1, mp - 1, mem)
    case s if pg(pc) == '+' => compute2(pg, tb,pc + 1, mp, write(mem, mp, sread(mem,mp) + 1))
    case s if pg(pc) == '-' => compute2(pg, tb,pc + 1, mp, write(mem, mp, sread(mem,mp) - 1))
    case s if pg(pc) == '.' => {
        print(sread(mem,mp).toChar)
        compute2(pg, tb,pc + 1, mp, mem)
    }
    case s if pg(pc) == '[' && sread(mem,mp) == 0 => compute2(pg, tb,tb(pc), mp, mem)
    case s if pg(pc) == '[' && sread(mem,mp) != 0 => compute2(pg, tb,pc + 1, mp, mem)
    case s if pg(pc) == ']' && sread(mem,mp) != 0 => compute2(pg, tb,tb(pc), mp, mem)
    case s if pg(pc) == ']' && sread(mem,mp) == 0 => compute2(pg, tb,pc + 1, mp, mem)
    case _ => compute2(pg,tb,pc + 1,mp,mem)
}
def run2(pg: String, m: Mem = Map()) = {
    compute2(pg, jtable(pg), 0, 0, m)
}

// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (7) 
def optimise(s: String) : String = {
  val str =("""[^<>+\-.\[\]]""".r).replaceAllIn(s, "")
  ("""\[-\]""".r).replaceAllIn(str, "0")
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = pg match {
    case s if pc == pg.length || pc == -1 => mem
    case s if pg(pc) == '>' => compute3(pg, tb,pc + 1, mp + 1, mem)
    case s if pg(pc) == '<' => compute3(pg, tb,pc + 1, mp - 1, mem)
    case s if pg(pc) == '+' => compute3(pg, tb,pc + 1, mp, write(mem, mp, sread(mem,mp) + 1))
    case s if pg(pc) == '-' => compute3(pg, tb,pc + 1, mp, write(mem, mp, sread(mem,mp) - 1))
    case s if pg(pc) == '0' => compute3(pg, tb, pc + 1, mp, write(mem, mp, 0))
    case s if pg(pc) == '.' => {
        print(sread(mem,mp).toChar)
        compute3(pg, tb,pc + 1, mp, mem)
    }
    case s if pg(pc) == '[' && sread(mem,mp) == 0 => compute3(pg, tb,tb(pc), mp, mem)
    case s if pg(pc) == '[' && sread(mem,mp) != 0 => compute3(pg, tb,pc + 1, mp, mem)
    case s if pg(pc) == ']' && sread(mem,mp) != 0 => compute3(pg, tb,tb(pc), mp, mem)
    case s if pg(pc) == ']' && sread(mem,mp) == 0 => compute3(pg, tb,pc + 1, mp, mem)
    case _ => compute3(pg,tb,pc + 1,mp,mem)
}

def run3(pg: String, m: Mem = Map()) = {
  val new_pg = optimise(pg)
  compute3(new_pg, jtable(new_pg), 0, 0, m)
}


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11205
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (8) 
//Turned these two functions into tail recursion
def counter(c: Char, s: String, acc: Int) : Int =  s match {
  case "" => acc
  case s if acc == 26 => acc
  case s if s.head == c => counter(c, s.tail, acc + 1)
  case s => acc
}

def combine(s: String): String = {
  def combineTail(s: String, acc: String = ""): String = s match {
    case "" => acc
    case str if str.head == '+' => {
      val n = counter('+', s, 0)
      combineTail(str.substring(n), s"${acc}+${(n + 64).toChar}")
    }
    case str if str.head == '-' => {
      val n = counter('-', s, 0)
      combineTail(str.substring(n), s"${acc}-${(n + 64).toChar}")
    }
    case str if str.head == '<' => {
      val n = counter('<', s, 0)
      combineTail(str.substring(n), s"${acc}<${(n + 64).toChar}")
    }
    case str if str.head == '>' => {
      val n = counter('>', s, 0)
      combineTail(str.substring(n), s"${acc}>${(n + 64).toChar}")
    }
    case str => combineTail(str.tail, s"${acc}${str.head}")
  }

  combineTail(s)
}

// testcase
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = pg match {
    case s if pc == pg.length || pc == -1 => mem
    case s if pg(pc) == '>' => compute4(pg, tb, pc + 2, mp + (pg(pc+1).toInt - 64), mem)
    case s if pg(pc) == '<' => compute4(pg, tb, pc + 2, mp - (pg(pc+1).toInt - 64) , mem)
    case s if pg(pc) == '+' => compute4(pg, tb, pc + 2, mp, write(mem, mp, sread(mem,mp) + (pg(pc+1).toInt - 64) ))
    case s if pg(pc) == '-' => compute4(pg, tb, pc + 2, mp, write(mem, mp, sread(mem,mp) - (pg(pc+1).toInt - 64) ))
    case s if pg(pc) == '0' => compute4(pg, tb, pc + 1, mp, write(mem, mp, 0))
    case s if pg(pc) == '.' => {
        print(sread(mem,mp).toChar)
        compute4(pg, tb,pc + 1, mp, mem)
    }
    case s if pg(pc) == '[' && sread(mem,mp) == 0 => compute4(pg, tb, tb(pc), mp, mem)
    case s if pg(pc) == '[' && sread(mem,mp) != 0 => compute4(pg, tb, pc + 1, mp, mem)
    case s if pg(pc) == ']' && sread(mem,mp) != 0 => compute4(pg, tb, tb(pc), mp, mem)
    case s if pg(pc) == ']' && sread(mem,mp) == 0 => compute4(pg, tb, pc + 1, mp, mem)
    case _ => compute4(pg,tb,pc + 1,mp,mem)
}

// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = {
  val new_pg = combine(optimise(pg))
  compute4(new_pg, jtable(new_pg), 0, 0, m)
}


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))

//run3(load_bff("sierpinski.bf")) == run(load_bff("sierpinski.bf"))
//run3(load_bff("benchmark.bf")) == run(load_bff("benchmark.bf"))

// combine(optimise(load_bff("benchmark.bf"))).length == 134
// combine(optimise(load_bff("mandelbrot.bf"))).length == 6509

// run4("""+++++++++++
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


}

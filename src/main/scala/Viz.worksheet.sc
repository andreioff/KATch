import nkpl._

def parse(s: String) =
  Parser.parseExpr(s) match {
    case Parser.Expr.NKExpr(nk) => Runner.evalNK(Map(), nk)
  }

object GV {
  // graphviz construction helper object

  def genColor2(y: String) = {
    val x = y + "arsotienaorisetnarstadtt"
    val r = (x.hashCode % 256).abs
    val g = 200
    val b = (x.hashCode / 256 / 256 % 256).abs
    // Convert RGB to hex #RRGGBB
    if y == "a" then "skyblue"
    else if y == "b" then "#C1E1C1"
    else if y == "c" then "peachpuff"
    else f"#${r}%02x${g}%02x${b}%02x"
  }

  var syms = scala.collection.mutable.Map[Any, String]()
  def gensym(obj: Any) =
    syms.getOrElseUpdate(obj, s"n${syms.size}")

  def reset() =
    val n = syms.size
    syms.clear()
    for i <- 0 until n do syms(i) = s"dummy$i"

  val sb = new StringBuilder
  def line(text: String) =
    sb.append("    " + text + "\n")

  val edgeColor = "#555"

  def edge(a: Any, b: Any, label: String = "") =
    line(s"""${gensym(a)} -> ${gensym(b)} [label=" $label ", labelangle=-30, fontsize=12, arrowsize=0.5, color="$edgeColor"]""")

  def edgeNK(a: Any, b: Any, label: String = "") =
    line(s"""${gensym(a)} -> ${gensym(b)} [label=" $label ", labelangle=-30, fontsize=12, arrowsize=0.5, color="$edgeColor"]""")

  def defaultEdge(a: Any, b: Any, label: String = "") =
    // dotted edge
    line(s"""${gensym(a)} -> ${gensym(b)} [arrowsize=0.5, style=dashed, color="$edgeColor"]""")

  def automatonNode(a: Any, label: String = "", start: Boolean) =
    val color = if !start then "#D4B0E6" else "#FF9999"
    line(s"""${gensym(a)} [label="$label", shape=circle, width=0.3, fixedsize=true, style=filled, fillcolor="$color"]""")

  def testNode(a: Any, label: String = "") =
    if (label == "⊤") || (label == "⊥") then line(s"""${gensym(a)} [label="$label", shape=box, width=0.3, height=0.3, fixedsize=true]""")
    else line(s"""${gensym(a)} [label="$label", shape=circle, width=0.3, fixedsize=true, style=filled, fillcolor="${genColor2(label)}"]""")

  def mutNode(a: Any, label: String) =
    line(s"""${gensym(a)} [label="", shape=diamond, width=0.15, height=0.15, style=filled, fillcolor="${genColor2(label)}"]""")

  def sameRank(xs: List[Any]) =
    line(s"""{rank=same; ${xs.map(gensym).mkString("; ")}}""")

  def vizSP(sp: SP) =
    lazy val gv: SP => Unit = memoize { sp =>
      sp match
        case SP.True => testNode(sp, "⊤")
        case SP.False => testNode(sp, "⊥")
        case SP.Test(x, ys, default) =>
          val z = VarMap(x)
          testNode(sp, s"$z")
          ys.foreach { case (v, a) =>
            edge(sp, a, s"$v")
            gv(a)
          }
          defaultEdge(sp, default, s"≠")
          gv(default)
    }
    gv(sp)

  def vizSPP(spp: SPP) =
    var levels = Map[Any, Set[Any]]()
    lazy val gv: SPP => Unit = memoize { spp =>
      spp match
        case SPP.Diag => testNode(spp, "⊤")
        case SPP.False => testNode(spp, "⊥")
        case SPP.TestMut(x, branches, muts, default) =>
          val z = VarMap(x)
          levels = levels.updated(z, levels.getOrElse(z, Set()) + spp)
          testNode(spp, s"$z")
          branches.foreach { case (v, muts) =>
            mutNode((spp, v), s"$z")
            edge(spp, (spp, v), s"$v")
            muts.foreach { case (v2, spp2) =>
              levels = levels.updated(z + "mut", levels.getOrElse(z + "mut", Set()) ++ Set((spp, v)))
              edge((spp, v), spp2, s"$v2")
              gv(spp2)
            }
          }
          mutNode((spp, "default"), s"$z")
          defaultEdge(spp, (spp, "default"), s"≠")
          muts.foreach { case (v2, spp2) =>
            edge((spp, "default"), spp2, s"$v2")
            gv(spp2)
          }
          defaultEdge((spp, "default"), default, s"≠")
          // Make all muts the same rank
          levels = levels.updated(z + "mut", levels.getOrElse(z + "mut", Set()) ++ Set((spp, "default")))
          gv(default)
    }
    gv(spp)
    for (k, xs) <- levels do sameRank(xs.toList)

  def vizNK(e0: NK) =
    val seen = scala.collection.mutable.Set[NK]()
    val spps = scala.collection.mutable.Map[SPP, String]()
    def gensym(spp: SPP) =
      // create a letter in the alphabet
      spps.getOrElseUpdate(spp, (spps.size + 'A').toChar.toString)
    def iter(e: NK): Unit = {
      if seen.contains(e) then return
      seen += e
      GV.automatonNode(e, "δ", e == e0)
      for (e2, spp) <- Bisim.δ(e) do
        iter(e2)
        GV.edgeNK(e, e2, gensym(spp))
    }
    iter(e0)
    GV.save("viz/deriv/deriv")
    GV.reset()
    for (spp, name) <- spps do
      GV.vizSPP(spp)
      GV.save(s"viz/deriv/derivspp${name}")
      GV.reset()

  def output() =
    val text = s"""
digraph G {
${sb.toString()}
}"""
    sb.clear()
    text

  def save(file: String) =
    import java.io.File
    import java.time.Instant

    val text = output()
    val pw = new java.io.PrintWriter(s"$file.gv")
    pw.write(text)
    pw.close()
    import sys.process._
    s"dot -Tpdf $file.gv -o ${file}.pdf".!
    // output tikz
    s"dot -Ttikz $file.gv -o ${file}.tikz".!

  def show() =
    save("viz.gv")
    import sys.process._
    s"dot -Tpdf viz.gv -o viz.pdf".!
}

VarMap("a")
VarMap("b")
VarMap("c")

def testS(x: String, y: Val) = SPP.test(VarMap(x), y)
def testNES(x: String, y: Val) = SPP.testNE(VarMap(x), y)
def mutS(x: String, y: Val) = SPP.mut(VarMap(x), y)

def testSs(x: String, ys: List[Val]) = ys.foldRight(SPP.False: SPP) { case (y, sp) => SPP.union(testS(x, y), sp) }
def mutSs(x: String, ys: List[Val]) = ys.foldRight(SPP.False: SPP) { case (y, sp) => SPP.union(mutS(x, y), sp) }

val e = parse("(((@a←1 ⋅ @b←2 ⋅ @c←3 ⋅ δ)⋆) + ((@b=2 ⋅ @c=3 ⋅ δ)⋆))⋆")
// val e = parse("(@a←4 + @a←2 ⋅ δ)⋆⋅(@a=2?⋅δ)")

// Compute the derivatives of e

val d1 = Bisim.δ(e)

// for e <- seen do
//    val spp = Bisim.ε(e)
//    val name = gensym(spp)
//    gvSPP(spp)
//    GV.save(s"viz/deriv/derivspp${name}")
//    GV.reset()

GV.vizNK(e)
GV.save("viz/deriv")
GV.reset()

val e2 = parse("(@a←1 + @b←2   + δ)⋆")
GV.vizNK(e2)
GV.save("viz/deriv2")
GV.reset()

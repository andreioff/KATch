package nkpl

import java.awt.image.SinglePixelPackedSampleModel

/** Represents an outgoing transition structure of the automaton. In particular, a map `Map(e1 -> spp1, e2 -> spp2, ...)`, where `e1,e2,...` are of type `NK` and `spp1,spp2,...` are of type `SPP`, represents `spp1⋅δ⋅e1 + spp2⋅δ⋅e2 + ...`.
  *
  * We maintain the invariant that the `spp`'s are disjoint, in the sense that they are disjoint when seen as subsets of `Pk*Pk`.
  */
type SMap = Map[NK, SPP]
object SMap {
  val SDup: SMap = Map(One -> SPP.Diag)
  val SZero: SMap = Map()

  /** Adds a new element to the SMap and canonicalizes it.
    *
    * @param x
    *   The original SMap.
    * @param e
    *   The element to be added.
    * @param spp
    *   The SPP value.
    * @return
    *   The updated SMap.
    */
  def add(x: SMap, e: NK, spp: SPP): SMap =
    // When adding an entry to a SMap, the main difficulty is to maintain the invariant that the spp's are disjoint
    // We first handle some simple cases here, which are special cased for speed
    if spp eq SPP.False then return x
    // if x.keySet.contains(e) then return x.updated(e, SPP.union(x(e), spp))
    // We intersect the new spp with each of the existing spp's in ... + sppi⋅δ⋅ei + ...
    // and replace it with (sppi ∩ spp)⋅δ⋅(e+ei) + (sppi ∖ spp)⋅δ⋅ei, setting spp := spp ∖ sppi
    var s = spp
    var z: SMap = Map()
    def addEntry(e: NK, spp: SPP) =
      if !(spp eq SPP.False) then z = z.updated(e, SPP.union(z.getOrElse(e, SPP.False), spp))
    for (e2, spp2) <- x do
      val inter = SPP.intersection(s, spp2)
      val diff2 = SPP.difference(spp2, s)
      s = SPP.difference(s, spp2)
      addEntry(Sum(Set(e, e2)), inter)
      addEntry(e2, diff2)
    addEntry(e, s)
    z

  /** Reconstructs and returns a canonical representation of the given map `x`.
    *
    * @param x
    *   The map to be canonicalized.
    * @return
    *   The canonical representation of the map.
    */
  def canonicalize(x: Map[NK, SPP]): SMap =
    var z: SMap = SZero
    for (e, spp) <- x do z = add(z, e, spp)
    z

  /** Computes the union of two SMap objects.
    *
    * @param x
    *   The first SMap object.
    * @param y
    *   The second SMap object.
    * @return
    *   The union of the two SMap objects.
    */
  def union(x: SMap, y: SMap): SMap =
    var z: SMap = x
    for (e, spp) <- y do z = add(z, e, spp)
    z

  /** Computes the intersection of two SMaps.
    *
    * @param x
    *   The first SMap.
    * @param y
    *   The second SMap.
    * @return
    *   The intersection of x and y as a new SMap.
    */
  def intersection(x: SMap, y: SMap): SMap =
    var s: SMap = Map()
    for (e1, spp1) <- x do
      for (e2, spp2) <- y do
        val inter = SPP.intersection(spp1, spp2)
        s = add(s, Intersection(e1, e2), inter)
    s

  /** Computes the difference between two SMap objects.
    *
    * @param x
    *   The first SMap object.
    * @param y
    *   The second SMap object.
    * @return
    *   The difference between x and y as an SMap object.
    */
  def difference(x: SMap, y: SMap): SMap =
    var s: SMap = Map()
    for (e1, spp1) <- x do
      for (e2, spp2) <- y do
        val inter = SPP.intersection(spp1, spp2)
        s = add(s, Difference(e1, e2), inter)
    val all2 = y.map { (e, spp) => spp }.foldLeft(SPP.False: SPP)(SPP.union(_, _))
    for (e, spp) <- x do s = add(s, e, SPP.difference(spp, all2))
    s
  def xor(x: SMap, y: SMap): SMap = union(difference(x, y), difference(y, x))

  /** Applies sequential composition of a state map and a netkat expression. We may need to merge entries in the state map, if they are not disjoint after the composition.
    *
    * @param x
    *   The state map.
    * @param y
    *   The netkat expression.
    * @return
    *   The resulting state map after applying sequential composition.
    */
  def seqNK(x: SMap, y: NK): SMap =
    // We replace each mapping e -> spp with Seq(List(e, y)) -> spp
    // This can potentially cause merging of entries, e.g. if y is ∅
    // Therefore, we need to carefully merge entries
    var z: SMap = Map()
    for (e, spp) <- x do
      val e2 = Seq(List(e, y))
      z = z.updated(e2, SPP.union(z.getOrElse(e2, SPP.False), spp))
    z
  def seqSPP(x: SPP, y: => SMap): SMap =
    if x eq SPP.False then return SZero
    canonicalize(y.map { case (e, spp) => (e, SPP.seq(x, spp)) })
  def assertDisjoint(x: SMap) =
    for (e1, spp1) <- x do for (e2, spp2) <- x do if e1 != e2 then assert(SPP.intersection(spp1, spp2) eq SPP.False)
}

/** Bisimilarity algorithms.
  */
object Bisim {

  /** Performs an n-ary union using a divide and conquer approach.
    *
    * @param xs
    *   The array of SPP objects to perform the union on.
    * @param l
    *   The left index of the range to consider in the array.
    * @param r
    *   The right index of the range to consider in the array.
    * @return
    *   The result of the n-ary union operation.
    */
  def bigUnion(xs: Array[SPP], l: Int, r: Int): SPP =
    val s = r - l
    if s <= 0 then SPP.False
    else if s == 1 then xs(l)
    else
      val m = l + s / 2
      SPP.union(bigUnion(xs, l, m), bigUnion(xs, m, r))

  /** Computes and returns the dup-free component SPP for the given NK (NetKAT) expression.
    *
    * @param e
    *   The NK expression for which to compute the dup-free component.
    * @return
    *   The dup-free component of the given NK expression.
    */
  lazy val ε0: NK => SPP = memoize { e =>
    e match {
      case Dup => SPP.False
      case Test(x, v) => SPP.test(x, v)
      case TestNE(x, v) => SPP.testNE(x, v)
      case TestSP(sp) => SPP.fromSP(sp)
      case Mut(x, v) => SPP.mut(x, v)
      case Seq(es) =>
        if es.contains(Dup) then SPP.False
        else es.foldLeft(SPP.Diag: SPP) { (a, b) => SPP.seq(a, ε0(b)) }
      case Sum(es) =>
        val es2 = es.toArray.map(e => ε0(e))
        bigUnion(es2, 0, es2.length)
      case Difference(e1, e2) => SPP.difference(ε0(e1), ε0(e2))
      case Intersection(e1, e2) => SPP.intersection(ε0(e1), ε0(e2))
      case XOR(e1, e2) => SPP.xor(ε0(e1), ε0(e2))
      case Star(e) => SPP.star(ε0(e))
    }
  }

  /** Calculates the derivative of the NK expression, represented as an SMap.
    *
    * @param e
    *   The NK expression for which to calculate the derivative.
    * @return
    *   The derivative of the NK expression as an SMap.
    */
  lazy val δ0: NK => SMap = memoize { e =>
    e match {
      case Dup => SMap.SDup
      case Test(x, v) => SMap.SZero
      case TestNE(x, v) => SMap.SZero
      case TestSP(sp) => SMap.SZero
      case Mut(x, v) => SMap.SZero
      case Seq(es) =>
        es match {
          case Nil => SMap.SZero
          case e :: es => // δ(e es) = δ(e) es + ε(e) δ(es)
            SMap.union(SMap.seqNK(δ0(e), Seq(es)), SMap.seqSPP(ε0(e), δ0(Seq(es))))
        }
      case Sum(es) => es.foldLeft(SMap.SZero) { (a, b) => SMap.union(a, δ0(b)) }
      case Difference(e1, e2) => SMap.difference(δ0(e1), δ0(e2))
      case Intersection(e1, e2) => SMap.intersection(δ0(e1), δ0(e2))
      case XOR(e1, e2) => SMap.xor(δ0(e1), δ0(e2))
      case Star(e) => // δ(e*) = ε(e)* δ(e) e*
        SMap.seqNK(SMap.seqSPP(SPP.star(ε0(e)), δ0(e)), Star(e))
    }
  }

  /** A utility method for benchmarking the execution time of a function.
    *
    * @param msg
    *   The message to display when benchmarking.
    * @param f
    *   The function to be benchmarked.
    * @return
    *   The result of the function.
    */
  def benchmark[T](msg: String, f: => T): T = {
    val start = System.nanoTime()
    val y = f
    val end = System.nanoTime()
    y
  }

  /** Applies the δ function to the given NK expression and benchmarks the execution time.
    *
    * @param e
    *   the input NK expression
    * @return
    *   the result of applying the δ function
    */
  def δ(e: NK): SMap =
    val result = benchmark(s"δ", { δ0(e) })
    result

  /** Applies the ε operation to the given NK expression and benchmarks the execution time.
    *
    * @param e
    *   The input NK expression.
    * @return
    *   The result of applying the ε operation to the input expression.
    */
  def ε(e: NK): SPP =
    benchmark(s"ε", { ε0(e) })

  /** Array containing the names of frenetic variables. This is needed because Frenetic only supports a limited set of variables. When converting NKPL expressions to Frenentic, we choose variables from this set.
    */
  val freneticVars = Array("switch", "port", "vswitch", "vport", "vfabric", "ethSrc", "ethDst", "vlanId", "vlanPcp", "ethTyp", "ipProto", "ip4Src", "ip4Dst", "tcpSrcPort", "tcpDstPort")

  /** Converts a NetKAT expression to Frenetic.
    *
    * @param e
    *   The NetKAT expression to be converted.
    * @param s
    *   The StringBuilder to store the converted expression.
    */
  def toKatPrim(e: NK, s: StringBuilder): Unit =
    def getVar(n: Int): String =
      if n < freneticVars.length then freneticVars(n)
      else throw new Throwable(s"Unsupported variable $n")
    def getVal(n: Int): Int =
      if n < 0 then 1000 + n
      else n
    s.append("(")
    e match
      case Seq(es) =>
        if es.isEmpty then s.append("id")
        else
          var first = true;
          for e <- es do
            if !first then s.append(";")
            first = false
            toKatPrim(e, s)
      case Sum(es) =>
        if es.isEmpty then s.append("drop")
        else
          var first = true
          for e <- es do
            if !first then s.append("+")
            first = false
            toKatPrim(e, s)
      case Test(x, v) => s.append(s"filter ${getVar(x)} = ${getVal(v)}")
      case TestNE(x, v) => s.append(s"filter (not ${getVar(x)} = ${getVal(v)})")
      case Mut(x, v) => s.append(s"${getVar(x)} := ${getVal(v)}")
      case Dup => s.append("dup")
      case Star(e) =>
        toKatPrim(e, s)
        s.append("*")
      case Difference(e1, e2) => throw new Throwable("Difference not supported")
      case Intersection(e1, e2) => throw new Throwable("Intersection not supported")
      case XOR(e1, e2) => throw new Throwable("XOR not supported")
      case TestSP(sp) => throw new Throwable("TestSP not supported")
      // case Forward(e) => throw new Throwable("Forward not supported")
      // case Backward(e) => throw new Throwable("Backward not supported")
      case VarName(x) => throw new Throwable("VarName not supported")
      case _ => throw new Throwable(s"Unsupported expression: $e")
    s.append(")")

  /** Converts an NK expression to a Frenetic KAT representation.
    *
    * @param e
    *   the NK expression to convert
    * @return
    *   the KAT string representation of the expression
    */
  def toKat(e: NK): String =
    val s = new StringBuilder()
    toKatPrim(e, s)
    s.toString()

  /** Determines if two NK expressions are bisimilar. This also converts the expressions to KAT and writes them to files if the `convertToKat` option is set. The same goal can be achieved using forward/backward, but this can be more efficient, as it avoids the need to compute the forward/backward sets, and can do early stopping.
    *
    * @param e1
    *   The first NK expression.
    * @param e2
    *   The second NK expression.
    * @return
    *   `true` if the expressions are bisimilar, `false` otherwise.
    */
  def bisim(e1: NK, e2: NK): Boolean =
    val result = bisimPrim(e1, e2)
    if !Options.convertToKat then return result
    val dir = "kat"
    var files = List[String]()
    // write e1 and e2 to files
    for e <- List(e1, e2) do
      val kat = toKat(e)
      // val filename = kat.hashCode().toHexString
      // use sha instead
      val filename = java.security.MessageDigest.getInstance("SHA-256").digest(kat.getBytes("UTF-8")).map("%02x".format(_)).mkString
      val f = new java.io.FileWriter(s"$dir/${filename}.kat")
      f.write(kat)
      f.close()
      files = files :+ filename
    val fw = new java.io.FileWriter(Options.katIndex(), true) // true to append
    fw.write(s"${files(0)}.kat ${files(1)}.kat $result\n")
    fw.close()
    result

  /** Checks if two given NK expressions are bisimilar.
    *
    * @param e1
    *   The first NK expression.
    * @param e2
    *   The second NK expression.
    * @return
    *   `true` if the expressions are bisimilar, `false` otherwise.
    */
  def bisimPrim(e1: NK, e2: NK): Boolean = {
    import scala.collection.mutable.Queue
    var todo: Queue[(NK, SP, NK)] = Queue((e1, SP.True, e2))
    def enq(a: NK, sp: SP, b: NK): Unit =
      if sp eq SP.False then return
      todo.enqueue((a, sp, b))
    def deq() =
      val (a, sp, b) = todo.head
      val rest = todo.filter { (a2, sp2, b2) => a == a2 && b == b2 }
      todo = todo.filterNot { (a2, sp2, b2) => a == a2 && b == b2 }
      (a, SP.unionN(rest.map(_._2)), b)
    var done: Map[(NK, NK), SP] = Map()
    var i = 0
    val limit = 100000
    while (todo.nonEmpty && i < limit) {
      if (!Options.suppressOutput && false) println(s"\u001B[34mIteration $i \u001B[0m")
      i += 1
      val (e1, sp, e2) = deq()
      // println(s"Testing equivalence of ($e1, $sp, $e2)")
      val done12 = done.getOrElse((e1, e2), SP.False)
      val spRest = SP.difference(sp, done12)
      if !(spRest eq SP.False) then
        done = done.updated((e1, e2), SP.union(done12, spRest))
        // println(s"done: $done")
        // Check for ε equivalence
        val εe1 = ε(e1)
        val εe2 = ε(e2)
        val cmp = SPP.equivAt(spRest, εe1, εe2)
        // println(s"SPP.equivAt($spRest, $εe1, $εe2) = $cmp")
        if !cmp then return false

        // println(s"Enqueued ($a, $sp, $b)")
        // Add all pairs to the queue
        // println("Adding all pairs to the queue")
        // for (e1, spp1) <- δ(e1) do for (e2, spp2) <- δ(e2) do enq(e1, SP.intersection(SPP.run(spRest, spp1), SPP.run(spRest, spp2)), e2)
        for (e1, spp1) <- δ(e1) do for (e2, spp2) <- δ(e2) do enq(e1, SPP.run(spRest, SPP.intersection(spp1, spp2)), e2)
        val all1 = δ(e1).map { (e, sp) => sp }.foldLeft(SPP.False: SPP)(SPP.union(_, _))
        val all2 = δ(e2).map { (e, sp) => sp }.foldLeft(SPP.False: SPP)(SPP.union(_, _))
        // println(s"all1: $all1, all2: $all2, sp2: $sp2")
        // println(s"δe1: $δe1, δe2: $δe2")
        // println("Adding right zeroes")
        for (e1, spp1) <- δ(e1) do enq(e1, SPP.run(spRest, SPP.difference(spp1, all2)), Zero)
        // println("Adding left zeroes")
        for (e2, spp2) <- δ(e2) do enq(Zero, SPP.run(spRest, SPP.difference(spp2, all1)), e2)
    }
    if i == limit then throw new Throwable("Limit exceeded")
    true
  }

  /** Determines the possible output packets that a NK expression can produce.
    *
    * @param e
    *   the NK expression to evaluate
    * @return
    *   the set of possible output packets
    */
  def forward(e: NK): SP =
    import scala.collection.mutable.Queue
    var todo: Queue[(NK, SP)] = Queue((e, SP.True))
    def enq(a: NK, sp: SP): Unit =
      if sp eq SP.False then return
      todo.enqueue((a, sp))
    def deq() =
      val (a, sp) = todo.head
      val rest = todo.filter { (a2, sp2) => a == a2 }
      todo = todo.filterNot { (a2, sp2) => a == a2 }
      (a, SP.unionN(rest.map(_._2)))
    var done: Map[NK, SP] = Map()
    var i = 0
    val limit = 100000
    while (todo.nonEmpty && i < limit) {
      if (!Options.suppressOutput && false) println(s"\u001B[34mIteration $i \u001B[0m")
      // benchmark(
      // s"Iteration $i time", {
      i += 1
      val (e, sp) = deq()
      val done1 = done.getOrElse(e, SP.False)
      val spRest = SP.difference(sp, done1)
      if !(spRest eq SP.False) then done = done.updated(e, SP.union(done1, spRest))
      for (e, spp) <- δ(e) do enq(e, SPP.run(spRest, spp))
      // }
      // )
    }
    SP.unionN(done.map { (e, sp) => SPP.run(sp, ε(e)) })

  /** Produces reverse transitions of an automaton.
    *
    * @param e
    *   The automaton to compute reverse transitions for.
    * @return
    *   A map representing the reverse transitions, where the keys are the source states, and the values are maps representing the target states and the corresponding SPPs.
    */
  def revTrans(e: NK): Map[NK, Map[NK, SPP]] =
    // find the set of reverse transitions in the automaton
    var states = Set[NK]()
    var todo = Set(e)
    var trans = Map[NK, SMap]()
    while (todo.nonEmpty) {
      val e = todo.head
      todo = todo - e
      states = states + e
      for (e2, spp) <- δ(e) do
        if !states.contains(e2) then todo = todo + e2
        // add transition e2 --spp--> e to `trans`
        trans = trans.updated(e2, trans.getOrElse(e2, Map()) + (e -> spp))
    }
    trans

  /** Determines the set of input packets that aren't dropped by the NK expression.
    *
    * @param e
    *   The NK expression to evaluate.
    * @return
    *   The set of input packets that aren't dropped.
    */
  def backward(e: NK): SP =
    import scala.collection.mutable.Queue
    // first, we find all the reverse transitions in the automaton
    val T = revTrans(e)
    var todo: Queue[(NK, SP)] = Queue((T.keys ++ Set(e)).map(e => (e, SPP.pull(ε(e), SP.True))).toSeq: _*)
    def enq(a: NK, sp: SP): Unit =
      if sp eq SP.False then return
      todo.enqueue((a, sp))
    def deq() =
      val (a, sp) = todo.head
      val rest = todo.filter { (a2, sp2) => a == a2 }
      todo = todo.filterNot { (a2, sp2) => a == a2 }
      (a, SP.unionN(rest.map(_._2)))
    var done: Map[NK, SP] = Map()
    var i = 0
    val limit = 100000
    while (todo.nonEmpty && i < limit) {
      if (!Options.suppressOutput && false) println(s"\u001B[34mIteration $i \u001B[0m")
      i += 1
      val (e, sp) = deq()
      val done1 = done.getOrElse(e, SP.False)
      val spRest = SP.difference(sp, done1)
      if !(spRest eq SP.False) then
        done = done.updated(e, SP.union(done1, spRest))
        for (e2, spp) <- T.getOrElse(e, Map()) do enq(e2, SPP.pull(spp, spRest))
    }
    done.getOrElse(e, SP.False)
}

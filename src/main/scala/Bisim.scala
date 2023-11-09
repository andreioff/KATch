package nkpl

//  fdd1 e1
//  fdd2 e2

//  fdd1 ∩ fdd2 -> e1+e2
//  fdd1 - fdd2 -> e1
//  fdd2 - fdd1 -> e2

// FDD[T] = Map[T, FDD]

// This represents an outgoing transition structure of the automaton.
// In particular, a map Map(e1 -> spp1, e2 -> spp2, ...), where e1,e2,.. : NK and spp1,spp2,.. : SPP
// represents spp1⋅δ⋅e1 + spp2⋅δ⋅e2 + ...
// We maintain the invariant that the spp's are disjoint, in the sense that they are
// disjoint when seen as a subsets of Pk*Pk.
// TODO: is that necessary, or do they only have to be disjoint in their second component?
type SMap = Map[NK, SPP]
object SMap {
  val SDup: SMap = Map(One -> SPP.Diag)
  val SZero: SMap = Map()

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

  def canonicalize(x: Map[NK, SPP]): SMap =
    var z: SMap = SZero
    for (e, spp) <- x do z = add(z, e, spp)
    z

  def union(x: SMap, y: SMap): SMap =
    var z: SMap = x
    for (e, spp) <- y do z = add(z, e, spp)
    z

  def intersection(x: SMap, y: SMap): SMap =
    var s: SMap = Map()
    for (e1, spp1) <- x do
      for (e2, spp2) <- y do
        val inter = SPP.intersection(spp1, spp2)
        s = add(s, Intersection(e1, e2), inter)
    s
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

def logBisim(msg: String): Unit = ()

object Bisim {
  lazy val ε0: NK => SPP = memoize { e =>
    e match {
      case Dup => SPP.False
      case Test(x, v) => SPP.test(x, v)
      case TestNE(x, v) => SPP.testNE(x, v)
      case Mut(x, v) => SPP.mut(x, v)
      case Seq(es) =>
        if es.contains(Dup) then SPP.False
        else es.foldLeft(SPP.Diag: SPP) { (a, b) => SPP.seq(a, ε0(b)) }
      case Sum(es) => es.foldLeft(SPP.False: SPP) { (a, b) => SPP.union(a, ε0(b)) }
      case Difference(e1, e2) => SPP.difference(ε0(e1), ε0(e2))
      case Intersection(e1, e2) => SPP.intersection(ε0(e1), ε0(e2))
      case XOR(e1, e2) => SPP.xor(ε0(e1), ε0(e2))
      case Star(e) => SPP.star(ε0(e))
    }
  }

  lazy val δ0: NK => SMap = memoize { e =>
    e match {
      case Dup => SMap.SDup
      case Test(x, v) => SMap.SZero
      case TestNE(x, v) => SMap.SZero
      case Mut(x, v) => SMap.SZero
      case Seq(es) =>
        es match {
          case Nil => SMap.SZero
          case e :: es => // δ0(e es) = δ0(e) es + ε(e) δ0(es)
            SMap.union(SMap.seqNK(δ0(e), Seq(es)), SMap.seqSPP(ε0(e), δ0(Seq(es))))
        }
      case Sum(es) => es.foldLeft(SMap.SZero) { (a, b) => SMap.union(a, δ0(b)) }
      case Difference(e1, e2) => SMap.difference(δ0(e1), δ0(e2))
      case Intersection(e1, e2) => SMap.intersection(δ0(e1), δ0(e2))
      case XOR(e1, e2) => SMap.xor(δ0(e1), δ0(e2))
      case Star(e) => // δ0(e*) = ε(e)* δ0(e) e*
        SMap.seqNK(SMap.seqSPP(SPP.star(ε0(e)), δ0(e)), Star(e))
    }
  }

  def benchmark[T](msg: String, f: => T): T = {
    val start = System.nanoTime()
    val y = f
    val end = System.nanoTime()
    println(f"  $msg: ${(end - start) / 1000.0}%.2f μs")
    y
  }

  def δ(e: NK): SMap =
    val result = benchmark(s"δ", { δ0(e) })
    // SMap.assertDisjoint(result) // FIXME: remove this when we are sure that the invariant is maintained
    result

  def ε(e: NK): SPP =
    benchmark(s"ε", { ε0(e) })

  def bisim(e1: NK, e2: NK): Boolean = {
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
      println(s"\u001B[34mIteration $i \u001B[0m")
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
      println(s"\u001B[34mIteration $i \u001B[0m")
      i += 1
      val (e, sp) = deq()
      val done1 = done.getOrElse(e, SP.False)
      val spRest = SP.difference(sp, done1)
      if !(spRest eq SP.False) then
        done = done.updated(e, SP.union(done1, spRest))
        for (e, spp) <- δ(e) do enq(e, SPP.run(spRest, spp))
    }
    SP.unionN(done.values)

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
        todo = todo + e2
        // add transition e2 --spp--> e to `trans`
        trans = trans.updated(e2, trans.getOrElse(e2, Map()) + (e -> spp))
    }
    trans

  def backward(e: NK): SP =
    import scala.collection.mutable.Queue
    // first, we find all the reverse transitions in the automaton
    val T = revTrans(e)
    var todo: Queue[(NK, SP)] = Queue(T.keys.map(e => (e, SPP.pull(ε(e), SP.True))).toSeq: _*)
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
      println(s"\u001B[34mIteration $i \u001B[0m")
      i += 1
      val (e, sp) = deq()
      val done1 = done.getOrElse(e, SP.False)
      val spRest = SP.difference(sp, done1)
      if !(spRest eq SP.False) then
        done = done.updated(e, SP.union(done1, spRest))
        for (e2, spp) <- T(e) do enq(e2, SPP.run(spRest, spp))
    }
    done(e)
}
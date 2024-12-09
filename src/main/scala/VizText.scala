package nkpl

import scala.util.control.NonLocalReturns.*

/** Text vizualization object used for generating JSON encodings of SPPs. */
object TV {

  val sb = new StringBuilder
  def writePacket(pckt: String, default: String) =
    if (sb.length() > 0) {
      sb.append(", ")
    }
    sb.append("[")
    if (pckt.length() > 0) {
      sb.append(pckt)
    } else {
      sb.append(default)
    }
    sb.append("]")

  /** Build a JSON encoding of an SPP. */
  def vizSPP(spp: SPP) =
    lazy val gv: (String, SPP) => Unit = memoize2 { (currPacket, spp) => returning {
      spp match
        case SPP.Diag => writePacket(currPacket, "\"True\"")
        case SPP.False => writePacket(currPacket, "\"False\"")
        case SPP.TestMut(x, branches, muts, default) =>
          val z = VarMap(x)
          branches.foreach { case (v, muts) =>
            muts.foreach { case (v2, spp2) =>
              val newPacket = appendFieldMutStr(currPacket, z, v, v2)
              gv(newPacket, spp2)
            }
          }

          // Default mutations are already included in the branches, if they exist. Return early.
          if (branches.size > 0) then throwReturn(()) 

          muts.foreach { case (v2, spp2) =>
            val newPacket = appendFieldMutStr(currPacket, z, "?", v2)
            gv(newPacket, spp2)
          }
           
          // If no default mutations exist either, continue with the default value. 
          if (muts.size == 0) {
            gv(currPacket, default)
          }
    }}
    gv("", spp)

  def appendFieldMutStr(currPacket: String, fName: String, v1: Any, v2: Any) = 
    var comma = ""
    if (currPacket.length() > 0) {
      comma = ", " 
    }
    // val newField = s"$fName[$v1 -> $v2]"
    val newField = s"{\"field\": \"$fName\", \"oldValue\": \"$v1\", \"newValue\": \"$v2\"}"
    currPacket + comma + newField

  def output() =
    val text = sb.toString()
    sb.clear()
    "[" + text + "]"
}

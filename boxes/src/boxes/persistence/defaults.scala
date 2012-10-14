package boxes.persistence

import com.novus.salat.Context

package object defaults {
  private class PC(val aliases: ClassAliases, val codec: Codec[Any], val salatContext: Context) extends PersistenceContext
  
  implicit val defaultPersistenceContext: PersistenceContext = new PC(new ClassAliases(), new CodecByClass(), new Context {val name = "boxes"})
}
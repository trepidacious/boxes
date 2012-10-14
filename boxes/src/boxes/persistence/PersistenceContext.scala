package boxes.persistence

import com.novus.salat.Context

trait PersistenceContext {
  def aliases: ClassAliases
  def codec: Codec[Any]
  def salatContext: Context
}

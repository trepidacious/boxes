package boxes

trait Ledger {
	def editable(record:Int, field:Int):Boolean
	def apply(record:Int, field:Int):AnyRef
	def update(record:Int, field:Int, value:AnyRef)
  def fieldName(field:Int):String
  def fieldClass(field:Int):Class[_]
	def recordCount():Int
	def fieldCount():Int
}

//If a ledger change has occurred, then the contents of any cell may have changed.
//If the column count, names or classes have changed, columnsChanged is true, although
//columnsChanged may be true when these HAVEN'T changed.
//Similarly rowCountChanged will be true if the number of rows may have changed.
case class LedgerChange(columnsChanged:Boolean, rowCountChanged:Boolean)

case class RecordViewChange

trait RecordView[T] extends Box[RecordViewChange] {
  def editable(record:Int, field:Int, recordValue:T):Boolean
  def apply(record:Int, field:Int, recordValue:T):AnyRef
  def update(record:Int, field:Int, recordValue:T, fieldValue:AnyRef)
  def fieldName(field:Int):String
  def fieldClass(field:Int):Class[_]
  def fieldCount():Int
}

class ListLedger[T](list:ListRef[T], rView:RecordView[T]) extends Ledger {
  def editable(record:Int, field:Int) = rView.editable(record, field, list(record))
  def apply(record:Int, field:Int) = rView(record, field, list(record))
  def update(record:Int, field:Int, value:AnyRef) = rView(record, field, list(record)) = value
  def fieldName(field:Int) = rView.fieldName(field)
  def fieldClass(field:Int) = rView.fieldClass(field)
  def recordCount() = list().size
  def fieldCount() = rView.fieldCount
}

trait Lens[T, V<:AnyRef] {
  def apply(t:T):V
  def name():String
  def valueManifest():Manifest[V]
}
trait VarLens[T, V<:AnyRef] extends Lens[T, V] {
  def update(t:T, v:V)
}

object Lens {
  def apply[T, V<:AnyRef](name:String, read:(T=>V))(implicit valueManifest:Manifest[V]) = {
    new LensDefault[T, V](name, read)(manifest)
  }
}

class LensDefault[T, V<:AnyRef](val name:String, val read:(T=>V))(implicit val valueManifest:Manifest[V]) extends Lens[T, V] {
  def apply(t:T) = read(t)
}

class VarLensDefault[T, V<:AnyRef](val name:String, val read:(T=>V), val write:((T,V)=>Unit))(implicit val valueManifest:Manifest[V]) extends Lens[T, V] {
  def apply(t:T) = read(t)
  def update(t:T, v:V) = write(t, v)
}

class LensRecordView[T<:AnyRef](lenses:Lens[T,_<:AnyRef]*) extends RecordView[T] {

  //Note that in a RecordView with mutability, we would need to call Box methods,
  //but this view itself is immutable - the records may be mutable, but this is
  //irrelevant

  override def editable(record:Int, field:Int, recordValue:T) = lenses(field).isInstanceOf[VarLens[_,_]]
  override def apply(record:Int, field:Int, recordValue:T) = lenses(field).apply(recordValue)

  override def update(record:Int, field:Int, recordValue:T, fieldValue:AnyRef) = {
    lenses(field) match {
      case varLens:VarLens[_,_] => {
        if(!varLens.valueManifest.typeArguments.isEmpty) {
          throw new RuntimeException("Can only use VarLens in LensRecordView for non-generic types")
        } else if (!varLens.valueManifest.erasure.isAssignableFrom(fieldValue.getClass)) {
          throw new RuntimeException("Invalid value, expected a " + varLens.valueManifest.erasure + " but got a " + fieldValue.getClass)
        } else {
          varLens.asInstanceOf[VarLens[AnyRef, AnyRef]].update(recordValue, fieldValue)
        }
      }
      case _ => throw new RuntimeException("Code error - not a VarLens for field " + field + ", but tried to update anyway")
    }
  }

  override def fieldName(field:Int) = lenses(field).name
  override def fieldClass(field:Int) = lenses(field).valueManifest.erasure
  override def fieldCount() = lenses.size

}


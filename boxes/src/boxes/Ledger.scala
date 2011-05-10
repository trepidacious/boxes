package boxes

trait Ledger extends Box[LedgerChange] {
	def editable(record:Int, field:Int):Boolean
	def apply(record:Int, field:Int):Any
	def update(record:Int, field:Int, value:Any)
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
  def apply(record:Int, field:Int, recordValue:T):Any
  def update(record:Int, field:Int, recordValue:T, fieldValue:Any)
  def fieldName(field:Int):String
  def fieldClass(field:Int):Class[_]
  def fieldCount():Int
}

class ListLedger[T](list:ListRef[T], rView:RecordView[T]) extends Ledger {

  //This reaction allows us to change correctly when we see a change
  //to our list
  private val listChangeReaction = new Reaction() {
    private var lastProcessedChangeIndex = -1L
    override def respond() = {
      var rowCountChanged = false;
      var changed = false;
      for {
        queue <- list.changes
        (index, change) <- queue
      } {
        if (index > lastProcessedChangeIndex) {
          lastProcessedChangeIndex = index
          change match {
            case ReplacementListChange(f, i)  => changed = true
            //All changes except replacement may change size
            case _ => {
              changed = true
              rowCountChanged = true
            }
          }
        }
      }
      if (changed) {
        {()=>commitChange(LedgerChange(false, rowCountChanged))}
      } else {
        {()=>()}
      }
    }
    override def isView = true
  }
  Box.registerReaction(listChangeReaction)

  //This reaction allows us to change correctly when we see a change
  //to our RecordView
  private val recordViewChangeReaction = new Reaction() {
    override def respond() = {
      val changed = (!rView.changes.isEmpty)
      if (changed) {
        {()=>commitChange(LedgerChange(true, false))}
      } else {
        {()=>()}
      }
    }
    override def isView = true
  }
  Box.registerReaction(recordViewChangeReaction)

  private def commitChange(change:LedgerChange) {
    try {
      Box.beforeWrite(this)
      Box.commitWrite(this, change)
    } finally {
      Box.afterWrite(this)
    }
  }
  def update(record:Int, field:Int, value:Any) = {
    try {
      Box.beforeWrite(this)
      rView(record, field, list(record)) = value
      Box.commitWrite(this, LedgerChange(false, false))
    } finally {
      Box.afterWrite(this)
    }
  }

  def editable(record:Int, field:Int) = boxRead{rView.editable(record, field, list(record))}
  def apply(record:Int, field:Int) = boxRead{rView(record, field, list(record))}
  def fieldName(field:Int):String = boxRead{rView.fieldName(field)}
  def fieldClass(field:Int) = boxRead[Class[_]]{rView.fieldClass(field)}  //TODO work out why the explicit parametric type is needed
  def recordCount() = boxRead{list().size}
  def fieldCount() = boxRead{rView.fieldCount}

  private def boxRead[T](read: =>T):T = {
    try {
      Box.beforeRead(this)
      return read
    } finally {
      Box.afterRead(this)
    }
  }
}

/**
 * Lens allowing reading of a "property" of a particular
 * data item. Also associates a name and a class via a Manifest
 */
trait Lens[T, V] {
  def apply(t:T):V
  def name():String
  def valueManifest():Manifest[V]
}

/**
 * Lens that also allows changing of the value of a property (mutation)
 */
trait MLens[T, V] extends Lens[T, V] {
  def update(t:T, v:V):T
}

object Lens {
  def apply[T, V](name:String, read:(T=>V))(implicit valueManifest:Manifest[V]) = {
    new LensDefault[T, V](name, read)(manifest)
  }
}

object MLens {
  def apply[T, V](name:String, read:(T=>V), write:((T,V)=>Unit))(implicit valueManifest:Manifest[V]) = {
    new MLensDefault[T, V](name, read, write)(manifest)
  }
}

/**
 * MLens based on a VarGeneral and an access closure
 */
object VarLens {
  def apply[T, V](name:String, access:(T=>VarGeneral[V,_]))(implicit valueManifest:Manifest[V]) = {
    new MLensDefault[T, V](
      name,
      (t) => access(t).apply,
      (t, v) => access(t).update(v)
    )(valueManifest)
  }
}

/**
 * MLens based on a RefGeneral and an access closure
 */
object RefLens {
  def apply[T, V](name:String, access:(T=>RefGeneral[V,_]))(implicit valueManifest:Manifest[V]) = {
    new LensDefault[T, V](
      name,
      (t) => access(t).apply
    )(valueManifest)
  }
}


class LensDefault[T, V](val name:String, val read:(T=>V))(implicit val valueManifest:Manifest[V]) extends Lens[T, V] {
  def apply(t:T) = read(t)
}

class MLensDefault[T, V](val name:String, val read:(T=>V), val write:((T,V)=>Unit))(implicit val valueManifest:Manifest[V]) extends MLens[T, V] {
  def apply(t:T) = read(t)
  def update(t:T, v:V) = {
    write(t, v)
    t
  }
}

object LensRecordView {
  def apply [T](lenses:Lens[T,_]*) = new LensRecordView[T](lenses:_*)
}

class LensRecordView[T](lenses:Lens[T,_]*) extends RecordView[T] {

  //Note that in a RecordView with mutability, we would need to call Box methods,
  //but this view itself is immutable - the records may be mutable, but this is
  //irrelevant

  override def editable(record:Int, field:Int, recordValue:T) = lenses(field).isInstanceOf[MLens[_,_]]
  override def apply(record:Int, field:Int, recordValue:T) = lenses(field).apply(recordValue)

  override def update(record:Int, field:Int, recordValue:T, fieldValue:Any) = {
    lenses(field) match {
      case mLens:MLens[_,_] => {
        fieldValue match {

          //TODO there HAS to be a better way to do this. The problem is that the AnyVals don't have getClass, so
          //we need to match to get the class, then pass it through. At least there is a known, fixed set of classes
          //here, and we know they must match the manifest exactly
          case v:Boolean => tryUpdate(mLens, recordValue, fieldValue, classOf[Boolean])
          case v:Byte => tryUpdate(mLens, recordValue, fieldValue, classOf[Byte])
          case v:Char => tryUpdate(mLens, recordValue, fieldValue, classOf[Char])
          case v:Double => tryUpdate(mLens, recordValue, fieldValue, classOf[Double])
          case v:Long => tryUpdate(mLens, recordValue, fieldValue, classOf[Long])
          case v:Int => tryUpdate(mLens, recordValue, fieldValue, classOf[Int])
          case v:Short => tryUpdate(mLens, recordValue, fieldValue, classOf[Short])

          //Now we have an AnyRef, it is much easier
          case fieldValueRef:AnyRef => {
            if(!mLens.valueManifest.typeArguments.isEmpty) {
              throw new RuntimeException("Can only use MLens in LensRecordView for non-generic types")
            } else if (!mLens.valueManifest.erasure.isAssignableFrom(fieldValueRef.getClass)) {
              throw new RuntimeException("Invalid value, expected a " + mLens.valueManifest.erasure + " but got a " + fieldValueRef.getClass)
            } else {
              mLens.asInstanceOf[MLens[Any, Any]].update(recordValue, fieldValueRef)
            }
          }

          case _ => throw new RuntimeException("Can't handle fieldValue " + fieldValue)
        }
      }
      case _ => throw new RuntimeException("Code error - not a MLens for field " + field + ", but tried to update anyway")
    }
  }

  private def tryUpdate(mLens:MLens[_,_], recordValue:T, fieldValue:Any, c:Class[_]) = {
    if (mLens.valueManifest.erasure == c) {
      mLens.asInstanceOf[MLens[Any, Any]].update(recordValue, fieldValue)
    } else {
      throw new RuntimeException("Invalid value, expected a " + mLens.valueManifest.erasure + " but got a " + c)
    }
  }

  override def fieldName(field:Int) = lenses(field).name
  override def fieldClass(field:Int) = lenses(field).valueManifest.erasure
  override def fieldCount() = lenses.size

}


package boxes

import list.{ReplacementListChange, ListRef, ListCal}
import scala.collection._
import boxes.list.ListUtils

//An table like view that has some immutability.
//Will always return the same results for fieldName,
//fieldClass, recordCount, fieldCount.
//apply and editable may return different values, but
//only if they delegate to state held in Boxes, in much the
//same way as an immutable List that may hold mutable instances.
//Update allows for creating a new Ledger with modified contents,
//much like the copy constructor of a case class.
//Where the Ledger DOES delegate to mutable Boxes, it may
//actually perform the update in place, and in this case should
//return the same Ledger from update.
//In general this complies with the requirements for data
//in the Box system, that data is either immutable, or is accessed
//via a Box and so tracked for reads and writes.
trait Ledger {
  def apply(record:Int, field:Int):Any
  def fieldName(field:Int):String
  def fieldClass(field:Int):Class[_]
  def recordCount():Int
  def fieldCount():Int
  
  def editable(record:Int, field:Int):Boolean
  def update(record:Int, field:Int, value:Any): Ledger
}

//A Box containing a Ledger, and showing changes when
//a new Ledger is set. Note that this does NOT necessarily show
//a change when the data that can be read from the Ledger itself
//changes, only when a new Ledger instance is present. LedgerChange
//gives some additional information on the differences between the
//old and new ledgers, in terms of column definitions and number of
//rows.
trait LedgerRef extends Box[Ledger, LedgerChange]{
  def apply(record:Int, field:Int):Any
  
  //TODO add these for convenience, delegating to contents? Note that
  //we shouldn't make LedgerRef extend Ledger though
//    def fieldName(field:Int):String
//  def fieldClass(field:Int):Class[_]
//  def recordCount():Int
//  def fieldCount():Int
//  
//  def editable(record:Int, field:Int):Boolean
//  def update(record:Int, field:Int, value:Any): Ledger

}

//TODO this should extend SingleChange, which will just be Change when we
//have only Ref-type Boxes and so all Boxes have single old and new values.
//If a ledger change has occurred, then the contents of any cell may have changed.
//If the column count, names or classes have changed, columnsChanged is true, although
//columnsChanged may be true when these HAVEN'T changed.
//Similarly rowCountChanged will be true if the number of rows may have changed.
case class LedgerChange(columnsChanged:Boolean, rowCountChanged:Boolean)

trait LedgerVal extends LedgerRef with ValBox[Ledger, LedgerChange]
object LedgerVal {
  def apply(l: Ledger) = new LedgerValDefault(l): LedgerVal
}
private class LedgerValDefault (private val t:Ledger) extends LedgerVal {
  def apply(record:Int, field:Int) = Box.read(this){t.apply(record, field)}
  def apply():Ledger = Box.read(this){t}
  override def toString = "LedgerVal(" + t + ")"
}

trait LedgerVar extends LedgerRef with VarBox[Ledger, LedgerChange] {
  def editable(record:Int, field:Int):Boolean
  def update(record:Int, field:Int, value:Any)
  def update(u : (Ledger => Option[(Ledger, LedgerChange)]))
}
object LedgerVar {
  def apply(l: Ledger) = new LedgerVarDefault(l): LedgerVar
}
private class LedgerVarDefault (private var t:Ledger) extends LedgerVar {
  def apply(record:Int, field:Int) = Box.read(this){t.apply(record, field)}
  def apply():Ledger = Box.read(this){t}

  def update(u : (Ledger => Option[(Ledger, LedgerChange)])) = {
    try {
      Box.beforeWrite(this)
      u.apply(t) match {
        case None => {}
        case Some((newT, c)) => {
          t = newT
          Box.commitWrite(this, c)
        }
      }
    } finally {
      Box.afterWrite(this)
    }
  }
  
  def update(newT: Ledger) = {
    try {
      Box.beforeWrite(this)
      if (newT != t) {
        val oldT = t
        t = newT        
        //TODO can we work out whether columns are the same? Is it
        //enough for the names, classes and count to match completely? 
        Box.commitWrite(this, LedgerChange(true, true))
      }
    } finally {
      Box.afterWrite(this)
    }
  }

  def editable(record:Int, field:Int) = Box.read(this)(t.editable(record, field))
  def update(record:Int, field:Int, value:Any) = update(t.update(record, field, value))

  override def toString = "LedgerVar(" + t + ")"
}

object LedgerCal {
  def apply[T](c: => Ledger) = {
    val v = LedgerVar(c)
    Reaction(v, c)
    v: LedgerRef
  }
}

//An immutable view of records of type T as a list of fields
trait RecordView[T] {
  def editable(record:Int, field:Int, recordValue:T):Boolean
  def apply(record:Int, field:Int, recordValue:T):Any
  def update(record:Int, field:Int, recordValue:T, fieldValue:Any)
  def fieldName(field:Int):String
  def fieldClass(field:Int):Class[_]
  def fieldCount():Int
}

object ListLedger {
  def apply[T](list:List[T], rView:RecordView[T]) = new ListLedger(list, rView)
}

class ListLedger[T](list:List[T], rView:RecordView[T]) extends Ledger {
  def apply(record:Int, field:Int) = rView(record, field, list(record))
  def fieldName(field:Int):String = rView.fieldName(field)
  def fieldClass(field:Int) = rView.fieldClass(field)
  def recordCount() = list.size
  def fieldCount() = rView.fieldCount
  
  def editable(record:Int, field:Int) = rView.editable(record, field, list(record))
  def update(record:Int, field:Int, value:Any) = {
    rView.update(record, field, list(record), value)
    this
  }
}

//Calculated LedgerVar that will always hold a ListLedger made from the
//current List and RecordView in the provided refs. Will also ensure that
//the LedgerVar shows LedgerChanges accurately but minimally reflecting
//the changes it shows, to allow for efficient viewing e.g. in a GUI
object ListLedgerVar {
  def apply[T](list: ListRef[T], rView:Box[RecordView[T], _]) = {
    val v = LedgerVar(ListLedger(list(), rView()))
    
    //On list change, update the ledger in the var if necessary,
    //making sure to do so with as small a LedgerChange as possible
    val listReaction = new Reaction(){

      private var lastProcessedChangeIndex = -1L

      def isView = false
      
      def react {
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
          v.update{_ => Some((ListLedger(list(), rView()), LedgerChange(false, rowCountChanged)))}
        }
      }
    }
    Box.registerReaction(listReaction);
    v.retainReaction(listReaction);
    
    //On record view change, update the ledger in the var if necessary,
    //making sure to do so with as small a LedgerChange as possible
    val recordViewReaction = new Reaction(){

      def isView = false
      
      def react {
        if (!rView.changes.isEmpty) {
          v.update{_ => Some((ListLedger(list(), rView()), LedgerChange(true, false)))}
        }
      }
    }
    Box.registerReaction(recordViewReaction);
    v.retainReaction(recordViewReaction);
    
    v
  }
  
}

object FieldCompositeLedger{
  def apply(ledgers:List[Ledger]) = new FieldCompositeLedger(ledgers)
}

class FieldCompositeLedger(val ledgers:List[Ledger]) extends Ledger {

  def recordCount = ledgers.foldLeft(ledgers.head.recordCount){(min, l) => math.min(l.recordCount, min)}
  def fieldCount = ledgers.foldLeft(0){(sum, l) => sum + l.fieldCount}
  private def cumulativeFieldCount = ledgers.scanLeft(0){(c, l) => c + l.fieldCount}.toList //Make cumulative field count, note starts with 0
  
  private def ledgerAndFieldAndLedgerIndex(field: Int): (Ledger, Int, Int) = {
    //Note that -1 is to allow for leading 0 in cumulativeFieldCount
    val ledgerIndex = cumulativeFieldCount.indexWhere(c => c > field) - 1

    //This happens if EITHER findIndexOf fails and returns -1, OR field is negative and so matches first entry in cumulativeFieldCount
    if (ledgerIndex < 0) throw new IndexOutOfBoundsException("Field " + field + " is not in composite ledger")

    (ledgers(ledgerIndex), field - cumulativeFieldCount(ledgerIndex), ledgerIndex)
  }
  
  def apply(record:Int, field:Int) = {
    val (l, f, _) = ledgerAndFieldAndLedgerIndex(field)
    l.apply(record, f)
  }
  
  def fieldName(field:Int) = {
    val (l, f, _) = ledgerAndFieldAndLedgerIndex(field)
    l.fieldName(f)
  }
  
  def fieldClass(field:Int) = {
    val (l, f, _) = ledgerAndFieldAndLedgerIndex(field)
    l.fieldClass(f)
  }
  
  def editable(record:Int, field:Int):Boolean = {
    val (l, f, _) = ledgerAndFieldAndLedgerIndex(field)
    l.editable(record, f)
  }
  
  def update(record:Int, field:Int, value:Any) = {
    val (l, f, li) = ledgerAndFieldAndLedgerIndex(field)
    val newLedger = l.update(record, f, value)
    //Optimisation for ledgers that just update mutable data
    //and return themselves - in that case we don't need to
    //make a new FieldCompositeLedger, since it would just
    //contain an equal list of ledgers anyway.
    if (newLedger == l) {
      this
    } else {
      val newList = ListUtils.replace(ledgers, li, newLedger)
      FieldCompositeLedger(newList)      
    }
  }
  
}

//Calculated LedgerVar that will always hold a ListLedger made from the
//current List and RecordView in the provided refs. Will also ensure that
//the LedgerVar shows LedgerChanges accurately but minimally reflecting
//the changes it shows, to allow for efficient viewing e.g. in a GUI
object FieldCompositeLedgerVar {
  def apply[T](ledgers: ListRef[LedgerRef]) = {
    
    val v = LedgerVar(FieldCompositeLedger(ledgers().map(_.apply())))    
    
    //When a component ledger changes, register a change
    //that covers all component ledgers' changes
    val ledgersReaction = new Reaction() {
      override def isView = false
      override def react() = {
  
        val allChanges = for {
          ledger <- ledgers()
          changeQueue <- ledger.changes.toSeq
          change <- changeQueue
        } yield (change._2)
  
        //Should we track last processed index?
        //See if any change affected columns, and same for rows.
        val (columns, rows) = allChanges.foldLeft((false, false)){(cAndR, c) => (cAndR._1 | c.columnsChanged, cAndR._2 | c.rowCountChanged)}
  
        if (columns || rows) {
          v.update{_ => Some(FieldCompositeLedger(ledgers().map(_.apply())), LedgerChange(columns, rows))}
        }  
      }
    }
    Box.registerReaction(ledgersReaction)
    v.retainReaction(ledgersReaction);
    
    v
  }
  
}


object DirectRecordView{
  def apply[T](fieldName: String)(implicit valueManifest:Manifest[T]) = new DirectRecordView(fieldName)(valueManifest)
}

class DirectRecordView[T](fieldName: String)(implicit valueManifest:Manifest[T]) extends RecordView[T] {
  def editable(record:Int, field:Int, recordValue:T) = false
  def apply(record:Int, field:Int, recordValue:T) = recordValue
  def update(record:Int, field:Int, recordValue:T, fieldValue:Any) {}
  def fieldName(field:Int):String = fieldName
  def fieldClass(field:Int):Class[_] = valueManifest.erasure
  def fieldCount() = 1
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
 * MLens based on a VarBox and an access closure
 */
object VarLens {
  def apply[T, V](name:String, access:(T=>VarBox[V,_]))(implicit valueManifest:Manifest[V]) = {
    new MLensDefault[T, V](
      name,
      (t) => access(t).apply,
      (t, v) => access(t).update(v)
    )(valueManifest)
  }
}

/**
 * MLens based on a Box and an access closure
 */
object RefLens {
  def apply[T, V](name:String, access:(T=>Box[V,_]))(implicit valueManifest:Manifest[V]) = {
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


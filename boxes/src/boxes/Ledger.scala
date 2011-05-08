package boxes

trait Ledger {
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
  def editable(record:Int, field:Int) = rView.editable(record, field, list(record))
  def apply(record:Int, field:Int) = rView(record, field, list(record))
  def update(record:Int, field:Int, value:Any) = rView(record, field, list(record)) = value
  def fieldName(field:Int) = rView.fieldName(field)
  def fieldClass(field:Int) = rView.fieldClass(field)
  def recordCount() = list().size
  def fieldCount() = rView.fieldCount
}

//class NodeRecordView[T <: Node](example:T) extends RecordView[T] {
//
//
//
//  def editable(record:Int, field:Int, recordValue:T):Boolean
//  def apply(record:Int, field:Int, recordValue:T):Any
//  def update(record:Int, field:Int, recordValue:T, fieldValue:Any)
//  def fieldName(field:Int):String
//  def fieldClass(field:Int):Class[_]
//  def fieldCount():Int
//}

//val view = View{
//  //Read all contents of list and convert to fields, so we have read
//  //enough to know fields
//  val fieldCount = rView.fieldCount
//  list().view.zipWithIndex foreach {
//    case (value, index) => for(field <- 0 until fieldCount) {
//      rView(index, field, value)
//    }
//  }
//}

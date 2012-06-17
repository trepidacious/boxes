package boxes

class BoxException extends RuntimeException {
}

class FailedReactionsException() extends BoxException {
}

class InvalidReactionException(s:String, r:Reaction, b:Box[_,_]) extends BoxException {

  override def toString = s + ":" + r

}

class InvalidReadException(b:Box[_,_]) extends BoxException {

  override def toString = "Invalid read of box " + b

}

class InvalidWriteException(b:Box[_,_]) extends BoxException {

  override def toString = "Invalid write of box " + b

}


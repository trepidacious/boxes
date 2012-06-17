package boxes.view

import boxes._
import boxes.swing._
import javax.swing.JPanel
import java.awt.BorderLayout
import scala.ref.WeakReference
import scala.collection._
import scala.collection.mutable.WeakHashMap

class MultiSwingView[M](model: Box[Option[M], _], viewSource: Box[Option[M], _]=>Option[SwingView]) extends SwingView {

  val component = new LinkingJPanel(this, new BorderLayout)

  private var subview = None: Option[SwingView]
  
  val view = View{
    //We request a new view whenever the entire value instance of the model
    //changes, any changes within that instance (the instance being mutated)
    //must be handled by the subview. Reading the model ref lets us be notified on these
    //changes.
    model()
    
    //Note that requesting the view and changing
    //to it are handled in the swing thread, so that we do not acquire any other
    //source dependencies as a reaction.
    replaceUpdate {changeToView()}
  }
  
  def changeToView() {
    val newView = viewSource.apply(model)
    
    //Handle swing mangling if necessary
    if (subview != newView) {
      component.removeAll();
      
      newView.foreach(v=>component.add(v.component()))

      component.revalidate();
      component.repaint();
      
      subview.foreach(_.component().revalidate())
      
      subview = newView
    }
  }
}

object ManifestFilterCal {
  def apply[T](source:Box[Option[_], _], default: T)(implicit manifest: Manifest[T]) = {
    def filter(of:Option[_]) = {
      //TODO should we just have an exception for parametric types? Should we have
      //special behaviour for e.g. Option, so that we can produce a Ref[T] where T is Option[V] ?
      for (o <- of if manifest.typeArguments.isEmpty && manifest.erasure.isInstance(o)) yield o.asInstanceOf[T]
    }
    Cal{filter(source()).getOrElse(default)}
  }
}

////Can be supplied as the source for a MultiSwingView, and can build new sources with further class to view mappings
//trait ViewSource extends Function1[Box[Option[AnyRef], _], Option[SwingView]] {
//  
//  //Add a new source of SwingViews, used for a specific model data type T. A default instance of T is required,
//  //which will be retained as long as the view is in use, so please use a minimal default instance.
//  def add[T <: AnyRef](source: Ref[T] => SwingView, default: T)(implicit manifest:Manifest[T]): ViewSource
//
//  //Produce a swing view capable of displaying all specific model data types added so far
//  def viewOf(model: Box[Option[AnyRef], _]): SwingView  
//}

object ViewSource{
  def byManifest() = new ViewSourceByManifest()
  def byInstance() = new ViewSourceByInstance()
}

class ViewSourceByManifest() extends Function1[Box[Option[AnyRef], _], Option[SwingView]]{
  
  private val sources = mutable.ListBuffer.empty[ManifestViewSource[_]]
  private val cachedViews = new mutable.HashMap[Class[_], SwingView]
  
  def makeView(ref: Box[Option[AnyRef], _]) = for {
    v <- ref()
    source <- sources.find(_.manifest.erasure.isInstance(v))
  } yield source.asInstanceOf[ManifestViewSource[Any]].view(ref)

  def apply(ref: Box[Option[AnyRef], _]) = {
    ref() match {
      case None => None
      case Some(v) => {
        cachedViews.get(v.getClass()) match {
          
          case Some(view) => Some(view)

          case None => {
            makeView(ref) match {
              case Some(view) => {
                cachedViews.put(v.getClass(), view)
                Some(view)
              }
              case None => None
            }
          }
          
        }
      }
    }
  }
  
  def add[T](source: Ref[T] => SwingView, default: T)(implicit manifest:Manifest[T]) = {
    sources += new ManifestViewSource[T](source, default)(manifest)
    this
  }
  
  def viewOf(model: Box[Option[AnyRef], _]) = new MultiSwingView(model, this): SwingView
}

private class ManifestViewSource[T](source: Ref[T] => SwingView, default: T)(implicit val manifest:Manifest[T]) {
  def view(ref: Box[Option[_], _]) = { 
    val tRef = ManifestFilterCal(ref, default)(manifest)
    source(tRef)
  }
}

class ViewSourceByInstance() extends Function1[Box[Option[AnyRef], _], Option[SwingView]] {
  
  private val sources = mutable.ListBuffer.empty[InstanceViewSource[_]]
  private val cachedViews = new mutable.WeakHashMap[AnyRef, SwingView]
  
  def makeView(ref: Box[Option[AnyRef], _]) = for {
    v <- ref()
    source <- sources.find(_.manifest.erasure.isInstance(v))  //TODO - some method of list that returns the first Some(x) result? This means the source implements the manifest check, which it should. Then implement that for manifest version as well.
    view <- source.asInstanceOf[InstanceViewSource[AnyRef]].view(ref)
  } yield view

  def apply(ref: Box[Option[AnyRef], _]) = {
    ref() match {
      case None => None
      case Some(v) => {
        cachedViews.get(v) match {
          
          case Some(view) => Some(view)

          case None => {
            makeView(ref) match {
              case Some(view) => {
                cachedViews.put(v, view)
                Some(view)
              }
              case None => None
            }
          }
          
        }
      }
    }

  }
  
  def add[T <: AnyRef](source: Ref[T] => SwingView, default: T)(implicit manifest:Manifest[T]) = {
    sources += new InstanceViewSource[T](source, default)(manifest)
    this
  }
  
  def viewOf(model: Box[Option[AnyRef], _]) = new MultiSwingView(model, this): SwingView
}

//A (very) specialised Cal, intended for use with view sources.
//In the case where we want to have a new view per viewed instance, we want to adapt a source
//Box of a type F into a Ref of a subclass T, for display by a specific SwingView implementation.
//However the resulting Ref[T] should NOT be just a Val(instance), since this would create a strong reference
//to instance. So instead, we accept a default value, and we create a Ref[T] that will contain instance when
//source contains the instance, and contain default otherwise. Hence it does not have a strong link to instance
//except when source does. The final part of this is to remember what the instance is - this is done via a
//WeakReference which will only survive as long as instance would be otherwise retained.
object WeakInstanceFilterCal {
  def apply[T <: AnyRef](source:Box[Option[_], _], default: T, instance: T)(implicit manifest: Manifest[T]) = {
    
    //Store the instance as a weak reference, since we don't want to retain
    //it if nothing else does
    val instanceWeak = new WeakReference(instance)
    Cal{
      
      //Some(instance) if instance weak reference and source both have values, and they are the same
      val instanceIfPossible = for {
        o <- source()
        i <- instanceWeak.get if o == i
        } yield i
      //Use instance if possible, otherwise the default
      instanceIfPossible.getOrElse(default)
    }
  }
}

private class InstanceViewSource[T <: AnyRef](source: Ref[T] => SwingView, default: T)(implicit val manifest:Manifest[T]) {
  def view(ref: Box[Option[_], _]) = {
    for (tVal <- ref() if manifest.erasure.isInstance(tVal)) yield source(WeakInstanceFilterCal(ref, default, tVal.asInstanceOf[T]))
  }
}


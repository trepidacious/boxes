package boxes.jfx

import java.util.ArrayList
import java.util.Collections
import java.util.List
import javafx.scene.control._
import javafx.event.ActionEvent
import javafx.geometry.Pos
import com.sun.javafx.css.StyleManager

object SlideCheck {
  private val DEFAULT_STYLE_CLASS = "slide-check";
  private val PSEUDO_CLASS_SELECTED = "selected";
  private val SELECTED_PSEUDOCLASS_STATE = StyleManager.getInstance().getPseudoclassMask("selected");
}

class SlideCheck extends ButtonBase {

    {
        getStyleClass().setAll(SlideCheck.DEFAULT_STYLE_CLASS);
        setAlignment(Pos.CENTER);
        setMnemonicParsing(true);     // enable mnemonic auto-parsing by default
    }

    override def fire() {
//        setSelected(!isSelected());
        fireEvent(new ActionEvent());
    }

//    private static final String DEFAULT_STYLE_CLASS = "toggle-button";

    def isSelected = false

    /**
     * @treatAsPrivate implementation detail
     * @deprecated This is an internal API that is not intended for use and will be removed in the next version
     */
    override def impl_getPseudoClassState(): Long = {
      super.impl_getPseudoClassState()
        var mask = super.impl_getPseudoClassState();
        if (isSelected) mask |= SlideCheck.SELECTED_PSEUDOCLASS_STATE;
        return mask;
    }
}

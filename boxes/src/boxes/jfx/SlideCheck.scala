package boxes.jfx

import javafx.event.ActionEvent
import javafx.geometry.Pos
import javafx.scene.control.ButtonBase

object SlideCheck {
  private val DEFAULT_STYLE_CLASS = "slide-check";
//  private val PSEUDO_CLASS_SELECTED = "selected";
//  private val SELECTED_PSEUDOCLASS_STATE = StyleManager.getInstance().getPseudoclassMask("selected");
}

class SlideCheck extends ButtonBase {

    {
        getStyleClass().setAll(SlideCheck.DEFAULT_STYLE_CLASS);
        setAlignment(Pos.CENTER);
        setMnemonicParsing(true);     // enable mnemonic auto-parsing by default
        setMinWidth(64)
    }

    override def fire() {
//        setSelected(!isSelected());
        fireEvent(new ActionEvent());
    }

//    private static final String DEFAULT_STYLE_CLASS = "toggle-button";

    def isSelected = false
}

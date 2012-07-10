package boxes.jfx

import javafx.scene.control.ToggleButton
import com.sun.javafx.scene.control.behavior.ButtonBehavior
import com.sun.javafx.scene.control.skin.LabeledSkinBase

class SlideCheckSkin(slideCheck: SlideCheck) extends LabeledSkinBase[SlideCheck, ButtonBehavior[SlideCheck]](slideCheck, new ButtonBehavior[SlideCheck](slideCheck)) {
}
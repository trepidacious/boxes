/*
 * Copyright (c) 2011, ScalaFX Project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the ScalaFX Project nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE SCALAFX PROJECT OR ITS CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scalafxtest

import collection.JavaConversions._
import javafx.scene.effect._
import javafx.scene.effect.BlendMode._
import javafx.scene.paint._
import javafx.scene.paint.Color._
import javafx.scene.paint.CycleMethod._
import javafx.scene.shape.StrokeType._
import scala.math.random
import scalafx.Includes._
import scalafx.animation.Timeline
import scalafx.animation.Timeline._
import scalafx.application.JFXApp
import scalafx.scene._
import scalafx.scene.paint.Stops
import scalafx.scene.shape._
import scalafx.stage.Stage

/**
 * SimpleColorfulCircles
 */
object SimpleColorfulCircles extends JFXApp {
  
  def makeCircles(c: Int, size: Double, blur: Option[BoxBlur], fade: Double): Seq[Circle] = for (i <- 0 until c) yield new Circle {
        centerX = random * 800
        centerY = random * 600
        radius = random * 150 * size
        fill = WHITE opacity 0.05 * fade
        stroke = WHITE opacity 0.16 * fade
        strokeWidth = 12 * size
        strokeType = OUTSIDE
        blur.foreach(effect = _)
      } 
  
  var circles: Seq[Circle] = Seq()
  stage = new Stage {
    width = 800
    height = 600
    scene = new Scene {
      fill = BLACK
//      circles = for (i <- 0 until 30) yield new Circle {
//        centerX = random * 800
//        centerY = random * 600
//        radius = random * 150
//        fill = WHITE opacity 0.05
//        stroke = WHITE opacity 0.16
//        strokeWidth = 12
//        strokeType = OUTSIDE
//        effect = new BoxBlur(10, 10, 3)
//      }
      circles = makeCircles(30, 1, None, 1) // ++ makeCircles(90, 0.3, None, 0.5) 
      content = circles :+ new Rectangle {
        width <== scene.width
        height <== scene.height
        fill = new LinearGradient(0, 1, 1, 0, true, NO_CYCLE,
          Stops(0xf8bd55, 0xc0fe56, 0x5dfbc1, 0x64c2f8, 0xbe4af7, 0xed5fc2, 0xef504c, 0xf2660f))
        blendMode = OVERLAY
      }
    }
  }
  new Timeline {
    cycleCount = INDEFINITE
    autoReverse = true
    keyFrames = (for (circle <- circles) yield at(40 s) {
      Set (
        circle.centerX -> random * 800,
        circle.centerY -> random * 600
      )
    })
  }.play();
}

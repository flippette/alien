/** This package provides an Akka-based mailbox and related tools for implementing
  * GUI events in [[o1.gui]]’s `View`s. It is used internally by both the mutable
  * and immutable `View` variants. */
package o1.gui.viewimpl

import o1.gui.*
import o1.gui.event.*
import o1.sound.sampled.Sound
import o1.gui.Tooltips
import o1.gui.View
import o1.gui.mutable.HasModelField
import o1.gui.viewimpl.tracing.*

/** This object provides internal tooling for [[o1.gui]]’s `View`s. */
object default:

  def unimplementedDefaultHandler: Nothing = throw View.NoHandlerDefined

  object mutable:

    trait Controls[Model] extends View.Controls[Model], Tooltips.Fast, HasModelField[Model]:
      private[gui] type Traced[TraceData] <: GeneratesTrace[Model,TraceData]
      private[gui] def tracedWith[TraceData](extractTrace: Model=>TraceData): Traced[TraceData]

      /** Returns a `View` that stores a pictorial trace of the ticks and GUI events that
        * the `View`’s event handlers process. This is equivalent to calling [[tracedWith]]
        * and passing in the `View`’s `makePic` method. */
      final def tracedPics: Traced[Pic] = this.tracedWith(this.makePic)

      /** Returns a `View` that stores a trace of the ticks and GUI events that its event handlers
        * process. This parameterless method stores, at each event, the `toString` description of
        * the `View`’s (mutable) model. This is equivalent to calling [[tracedWith]] and passing in
        * that `toString` method. */
      final def traced: Traced[String] = this.tracedWith( _.toString )


      /** Indicates whether the view is paused. By default, always returns `false`.
        * @see [[o1.gui.View.HasPauseToggle HasPauseToggle]] */
      override def isPaused = super.isPaused


      /** Returns a [[Pic]] that graphically represents the current state of the view’s `model`
        * object. This method is automatically invoked by the view after GUI events and clock ticks.
        * Left abstract by this class so any concrete view needs to add a custom implementation.
        *
        * For best results, all invocations of this method on a single view object should return
        * `Pic`s of equal dimensions. */
      def makePic: Pic


      /** Determines if the given state is a “done state” for the view. By default, this is never
        * the case, but that behavior can be overridden.
        *
        * Once done, the view stops reacting to events and updating its graphics and may close
        * its GUI window, depending on the constructor parameters of the view. */
      def isDone = super.isDone(this.model)


      /** Determines whether the view should play a sound, given the current state of its model.
        * By default, no sounds are played.
        * @return a [[o1.sound.sampled.Sound Sound]] that the view should play; `None` if no
        *         sound is appropriate for the current state */
      def sound = super.sound(this.model)


      /** Causes an additional effect when the view is stopped (with `stop()`).
        * By default, this method does nothing. */
      override def onStop() = super.onStop()


      /** Programmatically requests an update to the graphics of the view (even though no
        * clock tick or triggering GUI event occurred). */
      def refresh(): Unit


      //////////////////////////      SIMPLE HANDLERS       //////////////////////////

      /** Causes an effect whenever the view’s internal clock ticks.
        * Does nothing by default but can be overridden. */
      def onTick(): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever the mouse cursor moves above the view.
        * Does nothing by default but can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onMouseMove(position: Pos): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever the mouse cursor is dragged above the view.
        * Does nothing by default but can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onMouseDrag(position: Pos): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever the mouse wheel is rotated above the view.
        * Does nothing by default but can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param rotation  the number of steps the wheel rotated (negative means up, positive down) */
      def onWheel(rotation: Int): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a mouse button is clicked (pressed+released, possibly multiple
        * times in sequence) above the view. Does nothing by default but can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onClick(position: Pos): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a mouse button is pressed down above the view.
        * Does nothing by default but can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onMouseDown(position: Pos): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a mouse button is released above the view.
        * Does nothing by default but can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onMouseUp(position: Pos): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a key on the keyboard is pressed down while the view
        * has the keyboard focus. Does nothing by default but can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param key  the key that was pressed down  */
      def onKeyDown(key: Key): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a key on the keyboard is released while the view
        * has the keyboard focus. Does nothing by default but can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param key  the key that was released  */
      def onKeyUp(key: Key): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a key on the keyboard is typed (pressed+released) while
        * the view has the keyboard focus. Does nothing by default but can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param character  the key that was typed */
      def onType(character: Char): Unit = unimplementedDefaultHandler


      //////////////////////////     FULL HANDLERS       //////////////////////////

      /** Causes an effect whenever the view’s internal clock ticks.
        * Does nothing by default but can be overridden.
        *
        * If you don’t need the number of the clock tick, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param time  the running number of the clock tick (the first tick being number 1, the second 2, etc.) */
      def onTick(time: Long): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever the mouse cursor moves above the view.
        * Does nothing by default but can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param event  the GUI event that caused this handler to be called */
      def onMouseMove(event: MouseMoved): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever the mouse cursor is dragged above the view.
        * Does nothing by default but can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param event  the GUI event that caused this handler to be called */
      def onMouseDrag(event: MouseDragged): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever the mouse wheel is rotated above the view.
        * Does nothing by default but can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param event  the GUI event that caused this handler to be called */
      def onWheel(event: MouseWheelMoved): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a mouse button is clicked (pressed+released, possibly multiple
        * times in sequence) above the view. Does nothing by default but can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param event  the GUI event that caused this handler to be called */
      def onClick(event: MouseClicked): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a mouse button is pressed down above the view.
        * Does nothing by default but can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param event  the GUI event that caused this handler to be called */
      def onMouseDown(event: MousePressed): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a mouse button is released above the view.
        * Does nothing by default but can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param event  the GUI event that caused this handler to be called */
      def onMouseUp(event: MouseReleased): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever the mouse cursor enters the view.
        * Does nothing by default but can be overridden.
        * @param event  the GUI event that caused this handler to be called */
      def onMouseEnter(event: MouseEntered): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever the mouse cursor exits the view.
        * Does nothing by default but can be overridden.
        * @param event  the GUI event that caused this handler to be called */
      def onMouseExit(event: MouseExited): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a key on the keyboard is pressed down while the view
        * has the keyboard focus. Does nothing by default but can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param event  the GUI event that caused this handler to be called */
      def onKeyDown(event: KeyPressed): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a key on the keyboard is released while the view
        * has the keyboard focus. Does nothing by default but can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param event  the GUI event that caused this handler to be called */
      def onKeyUp(event: KeyReleased): Unit = unimplementedDefaultHandler

      /** Causes an effect whenever a key on the keyboard is typed (pressed+released) while
        * the view has the keyboard focus. Does nothing by default but can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param event  the GUI event that caused this handler to be called */
      def onType(event: KeyTyped): Unit = unimplementedDefaultHandler


      final override private[gui] def makePic (model: Model) = this.makePic
      final override private[gui] def isDone  (model: Model) = this.isDone
      final override private[gui] def sound   (model: Model) = this.sound

      final override private[gui] def onTick(model: Model) = { this.onTick(); this.model }
      final override private[gui] def onTick(model: Model, time: Long) = { this.onTick(time); this.model }

      final override private[gui] def onMouseMove(model: Model, position: Pos)   = { this.onMouseMove(position); this.model }
      final override private[gui] def onMouseDrag(model: Model, position: Pos)   = { this.onMouseDrag(position); this.model }
      final override private[gui] def onWheel    (model: Model, rotation: Int)   = { this.onWheel(rotation)    ; this.model }
      final override private[gui] def onClick    (model: Model, position: Pos)   = { this.onClick(position)    ; this.model }
      final override private[gui] def onMouseDown(model: Model, position: Pos)   = { this.onMouseDown(position); this.model }
      final override private[gui] def onMouseUp  (model: Model, position: Pos)   = { this.onMouseUp(position)  ; this.model }
      final override private[gui] def onKeyDown  (model: Model, key: Key)        = { this.onKeyDown(key)       ; this.model }
      final override private[gui] def onKeyUp    (model: Model, key: Key)        = { this.onKeyUp(key)         ; this.model }
      final override private[gui] def onType     (model: Model, character: Char) = { this.onType(character)    ; this.model }

      final override private[gui] def onMouseMove (model: Model, event: MouseMoved     ) = { this.onMouseMove(event) ; this.model }
      final override private[gui] def onMouseDrag (model: Model, event: MouseDragged   ) = { this.onMouseDrag(event) ; this.model }
      final override private[gui] def onMouseEnter(model: Model, event: MouseEntered   ) = { this.onMouseEnter(event); this.model }
      final override private[gui] def onMouseExit (model: Model, event: MouseExited    ) = { this.onMouseExit(event) ; this.model }
      final override private[gui] def onMouseUp   (model: Model, event: MouseReleased  ) = { this.onMouseUp(event)   ; this.model }
      final override private[gui] def onMouseDown (model: Model, event: MousePressed   ) = { this.onMouseDown(event) ; this.model }
      final override private[gui] def onWheel     (model: Model, event: MouseWheelMoved) = { this.onWheel(event)     ; this.model }
      final override private[gui] def onClick     (model: Model, event: MouseClicked   ) = { this.onClick(event)     ; this.model }
      final override private[gui] def onKeyDown   (model: Model, event: KeyPressed     ) = { this.onKeyDown(event)   ; this.model }
      final override private[gui] def onKeyUp     (model: Model, event: KeyReleased    ) = { this.onKeyUp(event)     ; this.model }
      final override private[gui] def onType      (model: Model, event: KeyTyped       ) = { this.onType(event)      ; this.model }

    end Controls


    trait TraceGeneratingView[Model,TraceData] extends Controls[Model], GeneratesTrace[Model,TraceData]:
      private[gui] val source: Controls[Model]
      def makePic: Pic = source.makePic
      override def isDone: Boolean = source.isDone
      override def sound: Option[Sound] = source.sound

      import TraceEvent.*
      override def onTick(): Unit                          = { source.onTick(); log(this.model, Tick()) }
      override def onTick(time: Long): Unit                = { source.onTick(time); log(this.model, Tick(time)) }
      override def onMouseMove(position: Pos): Unit        = { source.onMouseMove(position); log(this.model, Input("mouse-move", position)) }
      override def onMouseDrag(position: Pos): Unit        = { source.onMouseDrag(position); log(this.model, Input("mouse-drag", position)) }
      override def onWheel(rotation: Int): Unit            = { source.onWheel(rotation); log(this.model, Input("wheel", rotation)) }
      override def onClick(position: Pos): Unit            = { source.onClick(position); log(this.model, Input("click", position)) }
      override def onMouseDown(position: Pos): Unit        = { source.onMouseDown(position); log(this.model, Input("mouse-down", position)) }
      override def onMouseUp(position: Pos): Unit          = { source.onMouseUp(position); log(this.model, Input("mouse-up", position)) }
      override def onKeyDown(key: Key): Unit               = { source.onKeyDown(key); log(this.model, Input("key-down", key)) }
      override def onKeyUp(key: Key): Unit                 = { source.onKeyUp(key); log(this.model, Input("key-up", key)) }
      override def onType(character: Char): Unit           = { source.onType(character); log(this.model, Input("type", character)) }
      override def onMouseMove(event: MouseMoved): Unit    = { source.onMouseMove(event); log(this.model, Input("mouse-move", event)) }
      override def onMouseDrag(event: MouseDragged): Unit  = { source.onMouseDrag(event); log(this.model, Input("mouse-drag", event)) }
      override def onMouseEnter(event: MouseEntered): Unit = { source.onMouseEnter(event); log(this.model, Input("mouse-enter", event)) }
      override def onMouseExit(event: MouseExited): Unit   = { source.onMouseExit(event); log(this.model, Input("mouse-exit", event)) }
      override def onMouseUp(event: MouseReleased): Unit   = { source.onMouseUp(event); log(this.model, Input("mouse-up", event)) }
      override def onMouseDown(event: MousePressed): Unit  = { source.onMouseDown(event); log(this.model, Input("mouse-down", event)) }
      override def onWheel(event: MouseWheelMoved): Unit   = { source.onWheel(event); log(this.model, Input("wheel", event)) }
      override def onClick(event: MouseClicked): Unit      = { source.onClick(event); log(this.model, Input("click", event)) }
      override def onKeyDown(event: KeyPressed): Unit      = { source.onKeyDown(event); log(this.model, Input("key-down", event)) }
      override def onKeyUp(event: KeyReleased): Unit       = { source.onKeyUp(event); log(this.model, Input("key-up", event)) }
      override def onType(event: KeyTyped): Unit           = { source.onType(event); log(this.model, Input("type", event)) }

    end TraceGeneratingView

  end mutable



  object immutable:

    trait Controls[Model] extends View.Controls[Model], Tooltips.Fast:

      private[gui] def tracedWith[TraceData](traceData: Model=>TraceData): Traced[TraceData]
      private[gui] type Traced[TraceData] <: GeneratesTrace[Model,TraceData]

      /** Returns a `View` that stores a pictorial trace of the ticks and GUI events that
        * the `View`’s event handlers process. This is equivalent to calling [[tracedWith]]
        * and passing in the `View`’s `makePic` method. */
      final def tracedPics: Traced[Pic] = this.tracedWith(this.makePic)

      /** Returns a `View` that stores a trace of the ticks and GUI events that its event handlers
        * process. This parameterless method stores, at each event, the (immutable) state of the
        * `View`’s model. This is equivalent to calling [[tracedWith]] and passing in `identity`. */
      final def traced: Traced[Model] = this.tracedWith(identity)


      /** Returns a [[Pic]] that graphically represents the given state of the view’s model.
        * This method is automatically invoked by the view after GUI events and clock ticks.
        * Left abstract by this class so any concrete view needs to add a custom implementation.
        *
        * For best results, all invocations of this method on a single view object should return
        * `Pic`s of equal dimensions.
        *
        * @param state  a state of the model to be displayed */
      def makePic(state: Model): Pic


      /** Determines if the given state is a “done state” for the view. By default, this is
        * never the case, but that behavior can be overridden.
        *
        * Once done, the view stops reacting to events and updating its graphics and may close
        * its GUi window, depending on the constructor parameters of the view.
        *
        * @param state  a state of the model (possibly a done state) */
      override def isDone(state: Model) = super.isDone(state)


      /** Indicates whether the view is paused. By default, always returns `false`.
        * @see [[o1.gui.View.HasPauseToggle HasPauseToggle]] */
      override def isPaused = super.isPaused


      /** Determines whether the view should play a sound, given a state of its model.
        * By default, no sounds are played.
        * @param state  a state of the model
        * @return a [[o1.sound.sampled.Sound Sound]] that the view should play;
        *         `None` if no sound is appropriate for the given state */
      override def sound(state: Model) = super.sound(state)


      /** Causes an additional effect when the view is stopped (with `stop()`).
        * By default, this method does nothing. */
      override def onStop() = super.onStop()


      //////////////////////////      SIMPLE HANDLERS       //////////////////////////

      /** Determines what state should follow the given one on a tick of the view’s internal
        * clock. By default, just returns the unchanged state, but this can be overridden.
        * @param previousState  the state of the model before the clock tick */
      def onTick(previousState: Model): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when the mouse cursor moves above
        * the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param state     the state of the model at the time of the move event
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onMouseMove(state: Model, position: Pos): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when the mouse cursor is dragged above
        * the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param state     the state of the model at the time of the drag event
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onMouseDrag(state: Model, position: Pos): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when the mouse wheel is rotated above
        * the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param state     the state of the model at the time of the wheel event
        * @param rotation  the number of steps the wheel rotated (negative means up, positive down) */
      def onWheel(state: Model, rotation: Int): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a mouse button is clicked
        * (pressed+relesed, possibly multiple times in sequence) above the view. By default,
        * just returns the unchanged state, but this can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param state     the state of the model at the time of the click event
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onClick(state: Model, position: Pos): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a mouse button is pressed down
        * above the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param state     the state of the model at the time of the mouse event
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onMouseDown(state: Model, position: Pos): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a mouse button is released above
        * the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param state     the state of the model at the time of the mouse event
        * @param position  the position of the mouse cursor relative to the view’s top left-hand corner */
      def onMouseUp(state: Model, position: Pos): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a key on the keyboard is pressed
        * down while the view has the keyboard focus. By default, just returns the unchanged state,
        * but this can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the keyboard event
        * @param key    the key that was pressed down */
      def onKeyDown(state: Model, key: Key): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a key on the keyboard is released
        * while the view has the keyboard focus. By default, just returns the unchanged state, but
        * this can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the keyboard event
        * @param key    the key that was released */
      def onKeyUp(state: Model, key: Key): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a key on the keyboard is typed
        * (pressed+released) while the view has the keyboard focus. By default, just returns the
        * unchanged state, but this can be overridden.
        *
        * If the desired behavior depends on detailed information about the GUI event, you
        * may want to implement the other method of the same name instead of this one.
        *
        * @param state      the state of the model at the time of the keyboard event
        * @param character  the key that was typed */
      def onType(state: Model, character: Char): Model = unimplementedDefaultHandler


      //////////////////////////     FULL HANDLERS       //////////////////////////

      /** Determines what state should follow the given one on a tick of the view’s internal
        * clock. By default, just returns the unchanged state, but this can be overridden.
        *
        * If you don’t need the number of the clock tick, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param previousState  the state of the model before the clock tick
        * @param time           the running number of the clock tick (the first tick being number 1, the second 2, etc.) */
      def onTick(previousState: Model, time: Long): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when the mouse cursor moves above
        * the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the move event
        * @param event  the GUI event that caused this handler to be called */
      def onMouseMove(state: Model, event: MouseMoved): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when the mouse cursor is dragged above
        * the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the drag event
        * @param event  the GUI event that caused this handler to be called */
      def onMouseDrag(state: Model, event: MouseDragged): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when the mouse wheel is rotated above
        * the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the wheel event
        * @param event  the GUI event that caused this handler to be called */
      def onWheel(state: Model, event: MouseWheelMoved): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a mouse button is clicked
        * (pressed+relesed, possibly multiple times in sequence) above the view. By default,
        * just returns the unchanged state, but this can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the click event
        * @param event  the GUI event that caused this handler to be called */
      def onClick(state: Model, event: MouseClicked): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a mouse button is pressed down
        * above the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the mouse event
        * @param event  the GUI event that caused this handler to be called */
      def onMouseDown(state: Model, event: MousePressed): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a mouse button is released above
        * the view. By default, just returns the unchanged state, but this can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the mouse event
        * @param event  the GUI event that caused this handler to be called */
      def onMouseUp(state: Model, event: MouseReleased): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when the mouse cursor enters the
        * view. By default, just returns the unchanged state, but this can be overridden.
        * @param state  the state of the model at the time of the mouse event
        * @param event  the GUI event that caused this handler to be called */
      def onMouseEnter(state: Model, event: MouseEntered): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when the mouse cursor exits the
        * view. By default, just returns the unchanged state, but this can be overridden.
        * @param state  the state of the model at the time of the mouse event
        * @param event  the GUI event that caused this handler to be called */
      def onMouseExit(state: Model, event: MouseExited): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a key on the keyboard is pressed
        * down while the view has the keyboard focus. By default, just returns the unchanged state,
        * but this can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the keyboard event
        * @param event  the GUI event that caused this handler to be called */
      def onKeyDown(state: Model, event: KeyPressed): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a key on the keyboard is released
        * while the view has the keyboard focus. By default, just returns the unchanged state, but
        * this can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the keyboard event
        * @param event  the GUI event that caused this handler to be called */
      def onKeyUp(state: Model, event: KeyReleased): Model = unimplementedDefaultHandler

      /** Determines what state should follow the given one when a key on the keyboard is typed
        * (pressed+released) while the view has the keyboard focus. By default, just returns the
        * unchanged state, but this can be overridden.
        *
        * If you don’t need much information about the GUI event, you may find it simpler
        * to implement the other method of the same name instead of this one.
        *
        * @param state  the state of the model at the time of the keyboard event
        * @param event  the GUI event that caused this handler to be called */
      def onType(state: Model, event: KeyTyped): Model = unimplementedDefaultHandler

    end Controls


    trait TraceGeneratingView[Model <: Matchable,TraceData] extends Controls[Model], GeneratesTrace[Model,TraceData]:

      /** Returns a [[Pic]] that graphically represents the current state of the view’s `model` object.
        * This implementation delegates to the underlying `View` that is being traced.
        * @param state  a state of the model to be displayed */
      def makePic(state: Model): Pic = source.makePic(state)

      /** Determines if the given state is a “done state” for the view.
        * This implementation delegates to the underlying `View` that is being traced.
        * @param state  a state of the model (possibly a done state) */
      override def isDone(state: Model): Boolean = source.isDone(state)

      /** Determines whether the view should play a sound, given a state of its model.
        * This implementation delegates to the underlying underlying `View` that is being traced.
       *  @param state  a state of the model */
      override def sound(state: Model): Option[Sound] = source.sound(state)

      import TraceEvent.*
      /** Handles a clock tick and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onTick(previousState: Model): Model                    = log(source.onTick(previousState), Tick())
      /** Handles a clock tick and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onTick(previousState: Model, time: Long): Model        = log(source.onTick(previousState, time), Tick(time))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseMove(state: Model, position: Pos): Model        = log(source.onMouseMove(state, position), Input("mouse-move", position))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseDrag(state: Model, position: Pos): Model        = log(source.onMouseDrag(state, position), Input("mouse-drag", position))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onWheel(state: Model, rotation: Int): Model            = log(source.onWheel(state, rotation), Input("wheel", rotation))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onClick(state: Model, position: Pos): Model            = log(source.onClick(state, position), Input("click", position))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseDown(state: Model, position: Pos): Model        = log(source.onMouseDown(state, position), Input("mouse-down", position))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseUp(state: Model, position: Pos): Model          = log(source.onMouseUp(state, position), Input("mouse-up", position))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onKeyDown(state: Model, key: Key): Model               = log(source.onKeyDown(state, key), Input("key-down", key))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onKeyUp(state: Model, key: Key): Model                 = log(source.onKeyUp(state, key), Input("key-up", key))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onType(state: Model, character: Char): Model           = log(source.onType(state, character), Input("type", character))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseMove(state: Model, event: MouseMoved): Model    = log(source.onMouseMove(state, event), Input("mouse-move", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseDrag(state: Model, event: MouseDragged): Model  = log(source.onMouseDrag(state, event), Input("mouse-drag", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseEnter(state: Model, event: MouseEntered): Model = log(source.onMouseEnter(state, event), Input("mouse-enter", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseExit(state: Model, event: MouseExited): Model   = log(source.onMouseExit(state, event), Input("mouse-exit", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseUp(state: Model, event: MouseReleased): Model   = log(source.onMouseUp(state, event), Input("mouse-up", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onMouseDown(state: Model, event: MousePressed): Model  = log(source.onMouseDown(state, event), Input("mouse-down", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onWheel(state: Model, event: MouseWheelMoved): Model   = log(source.onWheel(state, event), Input("wheel", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onClick(state: Model, event: MouseClicked): Model      = log(source.onClick(state, event), Input("click", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onKeyDown(state: Model, event: KeyPressed): Model      = log(source.onKeyDown(state, event), Input("key-down", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onKeyUp(state: Model, event: KeyReleased): Model       = log(source.onKeyUp(state, event), Input("key-up", event))
      /** Handles a GUI event and adds it to the view’s [[trace]]. Delegates the actual event-handling
        * to the corresponding method on the underlying `View` that is being traced. */
      override def onType(state: Model, event: KeyTyped): Model           = log(source.onType(state, event), Input("type", event))

    end TraceGeneratingView

  end immutable

end default


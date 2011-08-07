package {

import flash.display.DisplayObject;
import flash.display.DisplayObjectContainer;
import flash.display.InteractiveObject;
import flash.display.MovieClip;
import flash.display.SimpleButton;
import flash.display.Sprite;
import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.MouseEvent;
import flash.text.TextField;

public class ViewController extends EventDispatcher {
	private var _view:Sprite;
	public static const disabledEvents:Array = 
		[Event.ADDED_TO_STAGE, MouseEvent.CLICK, MouseEvent.MOUSE_DOWN, MouseEvent.MOUSE_MOVE, 
		MouseEvent.ROLL_OUT, MouseEvent.ROLL_OVER, MouseEvent.MOUSE_OVER];
	
	public function ViewController(view:Sprite) {
		super();
		_view = view;
	}
	
	public function get view():Sprite {
		return _view;
	}

	protected function getChild(instanceName:String):DisplayObject {
		const result:DisplayObject = this.view.getChildByName(instanceName);
		if (result == null) {
			throw new Error("there is no child named " + instanceName);
		}
		return result;
	}

	protected function movieClip(instanceName:String):MovieClip {
		const result:MovieClip = getChild(instanceName) as MovieClip;
		if (result == null){
			throw new Error("there is no MovieClip "+instanceName)
		}
		return result
	}
	protected function sprite(instanceName:String):Sprite {
		return getChild(instanceName) as Sprite;
	}

	protected function simpleButton(instanceName:String):SimpleButton {
		return getChild(instanceName) as SimpleButton;
	}

	protected function textField(instanceName:String):TextField {
		return getChild(instanceName) as TextField;
	}

	protected function interactiveObject(instanceName:String):InteractiveObject {
		return getChild(instanceName) as InteractiveObject;
	}

	protected function displayObject(instanceName:String):DisplayObject {
		return getChild(instanceName) as DisplayObject;
	}

	protected function displayObjectContainer(instanceName:String):DisplayObjectContainer {
		return getChild(instanceName) as DisplayObjectContainer;
	}

	protected function validateEventType(type:String):void {
		if (disabledEvents.indexOf(type) != -1) {
			throw new Error("can not listen for DisplayObject-specific events!");
		}
	}

	override public function addEventListener(type:String, listener:Function, useCapture:Boolean = false, priority:int = 0, useWeakReference:Boolean = false):void {
		validateEventType(type);
		super.addEventListener(type, listener, useCapture, priority, useWeakReference);
	}

	override public function removeEventListener(type:String, listener:Function, useCapture:Boolean = false):void {
		validateEventType(type);
		super.removeEventListener(type, listener, useCapture);
	}
	
}
}

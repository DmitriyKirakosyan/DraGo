package core.component {
	
	import flash.events.Event;
	import flash.events.EventDispatcher;
	
	[Event(name="change", type="flash.events.Event")]
	
	public class CheckBoxBase extends EventDispatcher {
		
		protected var _selected:Boolean;
		protected var _enabled:Boolean = true;
		
		public function CheckBoxBase() {
		}
		
		public function swapSelected():void {
			selected = !selected;
		}
		
		public function get selected():Boolean { return _selected }
		public function set selected(value:Boolean):void {
			if (!_enabled || value == _selected) return;
			_selected = value;
			dispatchEvent(new Event(Event.CHANGE));
		}
		
		public function get enabled():Boolean { return _enabled }
		public function set enabled(value:Boolean):void {
			_enabled = value;
		}
		
	}
	
}

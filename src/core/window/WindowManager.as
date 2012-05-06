package core.window
{
	/**
	 * WindowManager
	 * @author satansdeer
	 */
	import core.event.WindowEvent;
	
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.events.EventDispatcher;
	import flash.events.IEventDispatcher;
	import flash.utils.Dictionary;
	
	public class WindowManager extends EventDispatcher{
		
		protected const registeredWindows:Dictionary = new Dictionary();
		protected const queue:Vector.<WindowBase> = new Vector.<WindowBase>();
		
		protected var currentWindow:WindowBase;
		
		protected var _layer:Sprite;
		
		private static var _instance:WindowManager;
		
		public function WindowManager() {
			super();
		}
		
		public static function get instance():WindowManager{
			if(!_instance){
				_instance = new WindowManager();
			}
			return _instance;
		}
		
		public function showWindow(name:String):void{
			show(getWindow(name));
		}

		public function hideWindow(name:String):void {
			if (currentWindow == getWindow(name)) {
				hideCurrentWindow();
			}
		}
		
		public function getWindow(name:String):WindowBase {
			if (!(name in registeredWindows)) {
				throw new Error("WindowController knows nothing about '" + name + "'");
			} else {
				return registeredWindows[name];
			}
		}
		
		public function registerWindow(name:String, window:WindowBase):void{
			registeredWindows[name] = window;
			window.addEventListener(WindowEvent.HIDE_AND_SHOW_NEXT_REQUEST, onHideWindow);
			window.addEventListener(WindowEvent.JUST_HIDE_REQUEST, onHideWindow);
		}
		
		protected function show(window:WindowBase):void {
			if (window is IScreenWindow) {
				forceShow(window);
			} else {
				scheduleShow(window);
			}
		}
		
		protected function forceShow(window:WindowBase):void {
			hideCurrentWindow();
			currentWindow = window;
			if(layer){
				layer.addChild(currentWindow);
			}else{
				throw(new Error("You must set layer before showing any windows"))
			}
		}
		
		protected function onHideWindow(event:Event):void{
			const window:WindowBase = event.target as WindowBase;
			if (window == currentWindow) {
				hideCurrentWindow();
				if (event.type == WindowEvent.HIDE_AND_SHOW_NEXT_REQUEST) {
					showNextInQueue();
				}
			}
		}
		
		protected function scheduleShow(window:WindowBase):void {
			if (currentWindow is IInformerWindow) {
				if (window is IInformerWindow) {
					queue.push(window);
				} else if (window is IScreenWindow) {
					queue.unshift(window);
				}
			} else {
				queue.push(window);
			}
		}
		
		public function showNextInQueue():void {
			
		}
		
		public function get layer():Sprite{
			return _layer;
		}
		
		public function hideCurrentWindow():void {
			if (currentWindow) {
				if (layer.contains(currentWindow)) {
					currentWindow.dispatchEvent(new WindowEvent(WindowEvent.ABOUT_TO_HIDE));
					layer.removeChild(currentWindow);
				}
				currentWindow = null;
			}
		}
		
		public function set layer(value:Sprite):void{
			_layer = value;
		}
	}
}
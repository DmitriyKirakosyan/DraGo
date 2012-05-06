package core.event {
	
	import flash.events.Event;
	
	import core.window.WindowBase;
	
	/**
	 * @author Viktor Kotseruba
	 */
	public class WindowEvent extends Event {
		
		public static const HIDE_AND_SHOW_NEXT_REQUEST:String = "hideAndSowNextRequest";
		public static const JUST_HIDE_REQUEST:String = "justHideRequest";
		public static const ABOUT_TO_SHOW:String = "aboutToShow";
		public static const ABOUT_TO_HIDE:String = "aboutToHide";
		public static const SHOW:String = "show";
		public static const ALL_WINDOWS_CLOSED:String = "allWindowsClosed";
		
		public function WindowEvent(type:String) {
			super(type);
		}
		
		public function get window():WindowBase {
			return target as WindowBase;
		}
		
	}
	
}

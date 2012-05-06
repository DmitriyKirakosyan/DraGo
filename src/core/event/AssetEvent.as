package core.event {
	
	import flash.events.Event;
	
	public class AssetEvent extends Event {
		
		public static const ASSET_LOADED:String = "assetLoaded";
		
		private var _url:String;
		
		public function AssetEvent(type:String, url:String) {
			_url = url;
			super(type);
		}
		
		public function get url():String { return _url }
		
	}
	
}
package rpc {
	import by.blooddy.crypto.serialization.JSON;
	
	import flash.events.EventDispatcher;
	import flash.utils.Dictionary;

	public class GameRpc extends EventDispatcher {
		private static var _instance:GameRpc;
		
		private var callbacks:Dictionary = new Dictionary();
		private var errbacks:Dictionary = new Dictionary();
		
		private var _rpc:RpcHttp;
		//private var connected:Boolean = false;
		private var userId:String;
		private var authKey:String;
		
		public static function get instance():GameRpc {
			if (!_instance) { _instance = new GameRpc(new ForSingleton()); }
			return _instance;
		}
		
		public function GameRpc(singleton:ForSingleton) {
			singleton;
			super();
		}
		
		public function init(host:String, port:int):void {
			_rpc = new RpcHttp(host, port);
		}
		
		public function authorize(callback:Function =null, errback:Function=null):void {
			_rpc.send("{" + getRequestString("authorize") + "}",
			function(result:Object):void {
				authKey = result.toString();
				if (callback) { callback(result); }
			});
		}

		public function getState(callback:Function):void {
			_rpc.send("{" + getRequestString("get_state") + "}", callback);
		}

		public function buyTown(id:String, x:int, y:int, callback:Function):void {
			var objectInfo:String = "\"id\" : " + id + ", \"x\" : " + x + ", \"y\" : " + y;
			_rpc.send("{" + getRequestString("buy_town") + ", " + objectInfo + "}", callback);
		}

		public function send(request:Object, callback:Function=null, errback:Function=null):void {
			_rpc.send(JSON.encode(request), callback);
		}

		/* Internal functions */

		private function getRequestString(requestName:String):String {
			var result:String = "\"request\" : \"" + requestName + "\"";
			if (authKey && authKey != "") {
				result += ", \"session_key\": \"" + authKey + "\"";
			}
			return result;
		}
		
	}
}

class ForSingleton{}

package rpc {
	import by.blooddy.crypto.serialization.JSON;
	
	import flash.events.EventDispatcher;
	import flash.utils.Dictionary;

	public class GameRpc extends EventDispatcher {
		private static var _instance:GameRpc;
		
		private var callbacks:Dictionary = new Dictionary();
		private var errbacks:Dictionary = new Dictionary();
		
		private var _rpc:RpcHttp;
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
		
		public function connect(userId:String, authKey:String, debug:Boolean = false):void {
			this.userId = userId;
			this.authKey = authKey;
			if (_rpc) { _rpc.send_test(); }
		}

		public function send(request:Object, callback:Function=null, errback:Function=null):void {
			_rpc.send(JSON.encode(request), callback);
		}
		
	}
}

class ForSingleton{}

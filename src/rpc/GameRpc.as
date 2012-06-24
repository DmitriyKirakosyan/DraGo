package rpc {
	import by.blooddy.crypto.serialization.JSON;
	
	import flash.events.EventDispatcher;

	public class GameRpc extends EventDispatcher {
		private static var _instance:GameRpc;
		
		private var _rpc:RpcHttp;
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

		public function makeMove(x:int, y:int, hidden:Boolean, callback:Function, errCallback:Function):void {
			send({request: "make_move", x: x,  y: y,  hidden: hidden}, callback, errCallback);
		}

		public function pass(callback:Function, errCallback:Function):void {
			send({request: "pass"}, callback, errCallback);
		}

		public function click_capture_stone(x:int, y:int, callback:Function, errCallback:Function):void {
			send({request: "click_capture_stone", x: x,  y: y}, callback, errCallback);
		}
		public function unclick_capture_stone(points:Array, callback:Function, errCallback:Function):void {
			send({request: "unclick_capture_stone", points: points}, callback, errCallback);
		}

		public function set_result_opinion(win:Boolean, callback:Function, errCallback:Function):void {
			send({request: "set_result_opinion", opinion: win ? "win" : "lose"}, callback, errCallback);
		}

		public function createRequest(userFor:String, callback:Function, errCallback:Function):void {
			send({request: "create_request", friend_user_id: userFor}, callback, errCallback);
		}
		public function approveRequest(owner:String, callback:Function):void {
			send({request: "approve_request", owner_user_id: owner}, callback);
		}
		public function declineRequest(owner:String, friend:String, callback:Function):void {
			send({request: "decline_request", owner_user_id: owner, friend_user_id: friend}, callback);
		}

		public function send(request:Object, callback:Function=null, errback:Function=null):void {
			if (authKey && authKey != "") {
				request["session_key"] = authKey;
			}
			_rpc.send(JSON.encode(request), callback, errback);
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

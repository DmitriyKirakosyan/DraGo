package rpc {
	import flash.net.URLRequestMethod;
	import flash.net.URLVariables;
	import flash.events.Event;
	import flash.net.URLRequest;
	import flash.net.URLLoader;
	import flash.events.EventDispatcher;

	public class RpcHttp extends EventDispatcher {
		private var _host:String;
		private var _port:int;
		
		private var _loader:URLLoader;
		
		public function RpcHttp(host:String, port:int, debug:Boolean = false) {
			super();
			_host = host;
			_port = port;
			_loader = new URLLoader();
		}
		
		public function send(jsonRequest:String, callback:Function):void {
			_loader.addEventListener(Event.COMPLETE, getOnLoadedFunction(callback));
			const request:URLRequest = new URLRequest("http://" + _host + ":" + _port);
			const vars:URLVariables = new URLVariables();
			vars.request = jsonRequest;
			request.method = URLRequestMethod.POST;
			request.data = vars;
			_loader.load(request);
		}
		
		public function send_test():void {
			_loader.addEventListener(Event.COMPLETE, onLoaded);
			const request:URLRequest = new URLRequest("http://" + _host + ":" + _port);
			const vars:URLVariables = new URLVariables();
			vars.tryit = "hi_server";
			request.method = URLRequestMethod.POST;
			request.data = vars;//"hi_server";
			_loader.load(request);
		}
		
		private function getOnLoadedFunction(callback:Function):Function {
			return function (event:Event):void { if (callback != null) callback(event.target.data); };
		}
		
		private function onLoaded(event:Event):void {
			event.target;
		}
		
	}
}

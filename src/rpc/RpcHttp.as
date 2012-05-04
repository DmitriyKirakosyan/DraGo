package rpc {
import by.blooddy.crypto.serialization.JSON;

import flash.net.URLRequestMethod;
import flash.net.URLVariables;
import flash.events.Event;
import flash.net.URLRequest;
import flash.net.URLLoader;
import flash.events.EventDispatcher;
import flash.utils.Dictionary;

public class RpcHttp extends EventDispatcher {
	private var _host:String;
	private var _port:int;

	private var _requests:Vector.<RequestVO>;
	//private var _loaders:Vector.<URLLoader>;

	public function RpcHttp(host:String, port:int, debug:Boolean = false):void {
		super();
		_requests = new Vector.<RequestVO>();
		_host = host;
		_port = port;
		//_loader = new URLLoader();
		//_loader.addEventListener(Event.COMPLETE, onLoadComplete);
	}

	public function send(jsonRequest:String, callback:Function):void {
		const urlLoader:URLLoader = new URLLoader();
		_requests.push(new RequestVO(urlLoader, callback));
		const request:URLRequest = new URLRequest("http://" + _host + ":" + _port);
		const vars:URLVariables = new URLVariables();
		vars["request"] = jsonRequest;
		request.method = URLRequestMethod.POST;
		request.data = vars;
		urlLoader.addEventListener(Event.COMPLETE, onLoadComplete);
		urlLoader.load(request);
	}

	private function onLoadComplete(event:Event):void {
		(event.target as URLLoader).removeEventListener(Event.COMPLETE, onLoadComplete);
		var callback:Function = getAndRemoveCallback(event.target as URLLoader);
		trace("response : " + event.target.data + " [RpcHttp.onLoadComplete]");
		if (callback != null) {
			var response:Object = JSON.decode(event.target.data)["response"];
			if (response && response["ok"]) {
				callback(response["ok"]);
			}
		}
	}

	private function getAndRemoveCallback(urlLoader:URLLoader):Function {
		for each (var request:RequestVO in _requests) {
			if (request.loader == urlLoader) {
				removeRequest(request);
				return request.callback;
			}
		}
		return null;
	}

	private function removeRequest(request:RequestVO):void {
		var index:int = _requests.indexOf(request);
		if (index != -1) {
			_requests.splice(index, 1);
		}
	}

	private function getOnLoadedFunction(callback:Function):Function {
		return function (event:Event):void { if (callback != null) callback(event.target.data); };
	}

	}
}

import flash.net.URLLoader;

class RequestVO {
	public var loader:URLLoader;
	public var callback:Function;

	public function RequestVO(loader:URLLoader, callback:Function):void {
		this.loader = loader;
		this.callback = callback;
	}
}

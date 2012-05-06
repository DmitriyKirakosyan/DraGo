/**
 * Created by : Dmitry
 * Date: 5/6/12
 * Time: 4:40 PM
 */
package game.manager {
import core.enum.WindowsENUM;
import core.window.WindowManager;

import flash.events.Event;

import game.iface.window.RequestWindow;

import game.request.RequestVO;

import game.request.RequestVO;

import game.staticModel.UserState;

import mx.skins.halo.WindowBackground;

import rpc.GameRpc;

public class RequestManager {
	private static var _instance:RequestManager;

	private var _currentRequest:RequestVO;

	private var _sendingRequest:Boolean;

	public static function get instance():RequestManager {
		if (!_instance) { _instance = new RequestManager(); }
		return _instance;
	}

	public function RequestManager():void {
		super();
		_sendingRequest = false;
	}

	public function init():void {
		addListeners();
	}

	public function makeRequest(userFor:String):void {
		var requestWindow:RequestWindow = WindowManager.instance.getWindow(WindowsENUM.REQUEST_WINDOW) as RequestWindow;
		requestWindow.setRequestByMeMode();
		WindowManager.instance.showWindow(WindowsENUM.REQUEST_WINDOW);
		GameRpc.instance.createRequest(userFor, onRequestCreated);
	}

	/* Internal functions */

	private function addListeners():void {
		UserState.instance.addEventListener(Event.CHANGE, onUserStateChange);
	}

	private function removeListeners():void {
		UserState.instance.removeEventListener(Event.CHANGE, onUserStateChange);
	}

	private function onRequestCreated(result:Object):void {
	}

	private function onUserStateChange(event:Event):void {
		if (!_currentRequest) {
			if (haveNewRequests()) {
				_currentRequest = getNewRequest();
				var requestWindow:RequestWindow = WindowManager.instance.getWindow(WindowsENUM.REQUEST_WINDOW) as RequestWindow;
				requestWindow.setRequestForMeMode(_currentRequest.owner);
				WindowManager.instance.showWindow(WindowsENUM.REQUEST_WINDOW);
			}
		} else {
			if (!UserState.instance.requests || UserState.instance.requests.indexOf(_currentRequest) == -1) {
				WindowManager.instance.hideWindow(WindowsENUM.REQUEST_WINDOW);
				_currentRequest = null;
			}
		}
	}

	private function haveNewRequests():Boolean {
		if (!UserState.instance.requests || UserState.instance.requests.length == 0) {
			return false;
		}
		for each (var requestVO:RequestVO in UserState.instance.requests) {
			if (requestVO.isNew) { return true; }
		}
		return false;
	}

	private function getNewRequest():RequestVO {
		var result:RequestVO;
		for each (var requestVO:RequestVO in UserState.instance.requests) {
			if (requestVO.isNew) {
				result = requestVO;
				break;
			}
		}
		return result;
	}
}
}

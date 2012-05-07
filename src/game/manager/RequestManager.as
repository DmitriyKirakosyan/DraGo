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

import game.staticModel.UserState;


import rpc.GameRpc;

public class RequestManager {
	private static var _instance:RequestManager;

	private var _currentRequest:RequestVO;

	private var _newRequests:Vector.<RequestVO>;

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
		GameRpc.instance.createRequest(userFor, onRequestCreated, onRequestFailed);
	}

	/* Internal functions */

	private function addListeners():void {
		UserState.instance.addEventListener(Event.CHANGE, onUserStateChange);
	}

	private function removeListeners():void {
		UserState.instance.removeEventListener(Event.CHANGE, onUserStateChange);
	}

	private function onRequestCreated(result:Object):void {
		_sendingRequest = true;
	}
	private function onRequestFailed(result:Object):void {
		WindowManager.instance.hideWindow(WindowsENUM.REQUEST_WINDOW);
	}

	private function onUserStateChange(event:Event):void {
		//hide window if my request failed
		if (_sendingRequest && !UserState.instance.requestsByMe) {
			_sendingRequest = false;
			WindowManager.instance.hideWindow(WindowsENUM.REQUEST_WINDOW);
		}

		//update new requests
		removeNotActualRequests();
		addNewRequests();

		//show request window if new request
		if (!_currentRequest && _newRequests && _newRequests.length > 0) {
			_currentRequest = _newRequests[0];
			var requestWindow:RequestWindow = WindowManager.instance.getWindow(WindowsENUM.REQUEST_WINDOW) as RequestWindow;
			requestWindow.setRequestForMeMode(_currentRequest.owner);
			WindowManager.instance.showWindow(WindowsENUM.REQUEST_WINDOW);
		}
	}

	private function removeNotActualRequests():void {
		var notActualRequests:Vector.<RequestVO> = new Vector.<RequestVO>();
		for each (var requestVO:RequestVO in _newRequests) {
			if (UserState.instance.requestsForMe.indexOf(requestVO.owner) == -1) {
				notActualRequests.push(requestVO);
			}
		}
		var index:int;
		for each (var notActualRequest:RequestVO in notActualRequests) {
			index = _newRequests.indexOf(notActualRequest);
			if (index != -1) {
				_newRequests.splice(index, 1);
			}
			if (notActualRequest == _currentRequest) {
				if (!(WindowManager.instance.getWindow(WindowsENUM.REQUEST_WINDOW) as RequestWindow).isRequestByMeMode()) {
					WindowManager.instance.hideWindow(WindowsENUM.REQUEST_WINDOW);
				}
				_currentRequest = null;
			}
		}
	}
	private function addNewRequests():void {
		for each (var requestOwner:String in UserState.instance.requestsForMe) {
			if (!getRequestVOByOwner(requestOwner)) {
				addNewRequest(requestOwner, UserState.instance.userId);
			}
		}
	}

	private function addNewRequest(owner:String, userFor:String):void {
		if (!_newRequests) { _newRequests = new Vector.<RequestVO>(); }
		_newRequests.push(new RequestVO(owner, userFor, false));
	}

	private function getRequestVOByOwner(owner:String):RequestVO {
		for each (var requestVO:RequestVO in _newRequests) {
			if (requestVO.owner == owner) { return requestVO; }
		}
		return null;
	}

}
}

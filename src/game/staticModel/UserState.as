/**
 * Created by : Dmitry
 * Date: 5/4/12
 * Time: 9:10 PM
 */
package game.staticModel {

import flash.events.Event;
import flash.events.EventDispatcher;

import flash.events.TimerEvent;
import flash.utils.Timer;

import game.request.RequestVO;

import rpc.GameRpc;

public class UserState extends EventDispatcher {
	private static var _instance:UserState;

	private var _timer:Timer;
	private var _sessionKey:String;
	private var _numUsers:int;

	//game requests
	private var _users:Array;
	private var _requestsForMe:Array;
	private var _requestsByMe:Array;

	//game
	private var _stones:Array;
	private var _movePlayer:String;
	private var _whiteUserId:String;
	private var _blackUserId:String;


	public static function get instance():UserState {
		if (!_instance) { _instance = new UserState(); }
		return _instance;
	}

	public function UserState() {
		super();
	}

	public function get users():Array { return _users; }
	public function get requestsForMe():Array { return _requestsForMe; }
	public function get requestsByMe():Array { return _requestsByMe}
	public function get userId():String { return _sessionKey; }
	public function get movePlayer():String { return _movePlayer; }
	public function get whiteUserId():String { return _whiteUserId; }
	public function get blackUserId():String { return _blackUserId; }
	public function get stones():Array { return _stones; }

	public function init(sessionKey:String):void {
		_sessionKey = sessionKey;
		_numUsers = 0;
		_timer = new Timer(2000);
		_timer.addEventListener(TimerEvent.TIMER, onTimer);
		_timer.start();
		GameRpc.instance.getState(onGetState);
	}

	private function onTimer(event:TimerEvent):void {
		GameRpc.instance.getState(onGetState);
	}

	private function onGetState(result:Object):void {
		_users = result["users"];
		updateRequests(result["requests"]);
		_movePlayer = result["move_player"];
		if (_movePlayer != null) {
			_stones = result["stones"];
			_whiteUserId = result["white_user_id"];
			_blackUserId = result["black_user_id"];
		}

		dispatchEvent(new Event(Event.CHANGE));
	}

	private function updateRequests(requestsObject:Object):void {
		if (!requestsObject) {
			_requestsByMe = null;
			_requestsForMe = null;
			return;
		}
		_requestsForMe = requestsObject["for_me"];
		_requestsByMe = requestsObject["by_me"];
	}

}
}

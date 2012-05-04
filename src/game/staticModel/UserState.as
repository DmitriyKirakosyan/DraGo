/**
 * Created by : Dmitry
 * Date: 5/4/12
 * Time: 9:10 PM
 */
package game.staticModel {
import controller.GameController;

import flash.events.TimerEvent;
import flash.utils.Timer;

import rpc.GameRpc;

public class UserState {
	private static var _instance:UserState;

	private var _timer:Timer;
	private var _sessionKey:String;
	private var _numUsers:int;


	public static function get instance():UserState {
		if (!_instance) { _instance = new UserState(); }
		return _instance;
	}

	public function UserState() {
		super();
	}


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
		trace("state got [UserState.onGetState]");
	}

}
}

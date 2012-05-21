/**
 * Created by : Dmitry
 * Date: 5/16/12
 * Time: 1:59 AM
 */
package game.manager {
import flash.events.Event;
import flash.events.EventDispatcher;

import game.events.MatchManagerEvent;

import game.staticModel.UserState;
import game.stone.StoneVO;

public class MatchManager extends EventDispatcher{
	private static var _instance:MatchManager;

	private var _started:Boolean;
	private var _stones:Vector.<StoneVO>;
	private var _movePlayer:String;
	private var _whiteUserId:String;
	private var _blackUserId:String;

	public static function get instance():MatchManager {
		if (!_instance) { _instance = new MatchManager(); }
		return _instance;
	}

	public function MatchManager() {
		super();
	}

	public function init():void {
		_started = false;
		_stones = new Vector.<StoneVO>();
		UserState.instance.addEventListener(Event.CHANGE, onStateUpdate);
	}

	public function getLastStone():StoneVO {
		if (_stones && _stones.length > 0) {
			return _stones[_stones.length-1];
		}
		return null;
	}

	private function onStateUpdate(event:Event):void {
		if (UserState.instance.movePlayer && UserState.instance.stones) {
			if (!_started) {
				_started = true;
				_whiteUserId = UserState.instance.whiteUserId;
				_blackUserId = UserState.instance.blackUserId;
				dispatchEvent(new MatchManagerEvent(MatchManagerEvent.GAME_STARTED));
			}

			updateStones();
			if (_movePlayer != UserState.instance.movePlayer) {
				_movePlayer = UserState.instance.movePlayer;
				dispatchEvent(new MatchManagerEvent(MatchManagerEvent.CHANGE_MOVE_PLAYER));
			}

		} else if (_started) {
			_started = false;
			dispatchEvent(new MatchManagerEvent(MatchManagerEvent.GAME_STOPPED));
		}
	}

	private function updateStones():void {
		var newStones:Vector.<StoneVO> = getNewStones();
			for each (var stoneVO:StoneVO in newStones) {
				addStone(stoneVO);
			}
	}

	private function getNewStones():Vector.<StoneVO> {
		var stoneObject:Object;
		var result:Vector.<StoneVO>;
		var remoteStonesLength:int = UserState.instance.stones.length;
		for (var i:int = 0; i < remoteStonesLength - _stones.length; ++i) {
			if (!result) { result = new Vector.<StoneVO>(); }
			result.push(StoneVO.createStoneByObject(UserState.instance.stones[0])); //only if player can only one move
		}
		return result;
	}

	private function addStone(stoneVO:StoneVO):void {
		_stones.push(stoneVO);
		dispatchEvent(new MatchManagerEvent(MatchManagerEvent.NEW_STONE));
		trace("new stone [MatchManager.addStone]");
	}

}
}

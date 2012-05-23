/**
 * Created by : Dmitry
 * Date: 5/16/12
 * Time: 1:59 AM
 */
package game.staticModel {
import flash.events.Event;
import flash.events.EventDispatcher;

import game.events.MatchManagerEvent;
import game.player.PlayerVO;

import game.staticModel.UserState;
import game.stone.StoneVO;

public class MatchState extends EventDispatcher{
	private static var _instance:MatchState;

	private var _started:Boolean;
	private var _stones:Vector.<StoneVO>;
	private var _movePlayer:String;
	private var _whitePlayer:PlayerVO;
	private var _blackPlayer:PlayerVO;

	public static function get instance():MatchState {
		if (!_instance) { _instance = new MatchState(); }
		return _instance;
	}

	public function MatchState() {
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

	public function get movePlayer():String { return _movePlayer; }

	public function get whitePlayer():PlayerVO {
		return _whitePlayer;
	}
	public function get blackPlayer():PlayerVO {
		return _blackPlayer;
	}

	private function onStateUpdate(event:Event):void {
		if (UserState.instance.game) {
			var game:Object = UserState.instance.game;
			if (!_started) {
				_started = true;
				_whitePlayer = new PlayerVO(game["white_user_id"], StoneVO.WHITE);
				_blackPlayer = new PlayerVO(game["black_user_id"], StoneVO.BLACK);
				dispatchEvent(new MatchManagerEvent(MatchManagerEvent.GAME_STARTED));
			}

			if (_movePlayer != game["move_player"]) {
				_movePlayer = game["move_player"];
				dispatchEvent(new MatchManagerEvent(MatchManagerEvent.CHANGE_MOVE_PLAYER));
			}
			updateStones(game["stones"]);

		} else if (_started) {
			_started = false;
			dispatchEvent(new MatchManagerEvent(MatchManagerEvent.GAME_STOPPED));
		}
	}

	private function updateStones(stones:Array):void {
		var newStones:Vector.<StoneVO> = getNewStones(stones);
			for each (var stoneVO:StoneVO in newStones) {
				addStone(stoneVO);
			}
	}

	private function getNewStones(stones:Array):Vector.<StoneVO> {
		var result:Vector.<StoneVO>;
		var remoteStonesLength:int = stones.length;
		for (var i:int = 0; i < remoteStonesLength - _stones.length; ++i) {
			if (!result) { result = new Vector.<StoneVO>(); }
			result.push(StoneVO.createStoneByObject(stones[0])); //only if player can only one move
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

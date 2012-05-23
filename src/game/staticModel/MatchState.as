/**
 * Created by : Dmitry
 * Date: 5/16/12
 * Time: 1:59 AM
 */
package game.staticModel {
import flash.events.Event;
import flash.events.EventDispatcher;

import game.events.MatchStateEvent;
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
	private var _phase:String;

	public static const BASIC_PHASE:String = "basic_phase";
	public static const MAIN_PHASE:String = "main_phase";
	public static const END_PHASE:String = "end_phase";

	public static const NUM_BASIC_STONES:int = 3;

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
	public function get phase():String { return _phase; }

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
				startMatch(game);
			}

			updateMovePlayer(game["move_player"]);
			updatePhase(game["phase"]);
			updateStones(game["stones"]);

		} else if (_started) {
			_started = false;
			finishMatch();
		}
	}

	private function startMatch(game:Object):void {
		_whitePlayer = new PlayerVO(game["white_user_id"], StoneVO.WHITE);
		_blackPlayer = new PlayerVO(game["black_user_id"], StoneVO.BLACK);
		dispatchEvent(new MatchStateEvent(MatchStateEvent.GAME_STARTED));
	}
	private function finishMatch():void {
		dispatchEvent(new MatchStateEvent(MatchStateEvent.GAME_STOPPED));
	}

	private function updateMovePlayer(movePlayer:String):void {
		if (_movePlayer != movePlayer) {
			_movePlayer = movePlayer;
			dispatchEvent(new MatchStateEvent(MatchStateEvent.CHANGE_MOVE_PLAYER));
		}
	}

	private function updatePhase(phase:String):void {
		if (!_phase || _phase != phase) {
			if (phase) {
				_phase = phase;
				dispatchEvent(new MatchStateEvent(MatchStateEvent.PHASE_CHANGED));
			}
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
		dispatchEvent(new MatchStateEvent(MatchStateEvent.NEW_STONE));
		trace("new stone [MatchManager.addStone]");
	}

}
}

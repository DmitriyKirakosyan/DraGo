/**
 * Created by : Dmitry
 * Date: 5/16/12
 * Time: 1:59 AM
 */
package game.staticModel {
import flash.events.Event;
import flash.events.EventDispatcher;
import flash.geom.Point;

import game.events.MatchStateClickEvent;

import game.events.MatchStateEvent;
import game.player.PlayerVO;

import game.staticModel.UserState;
import game.stone.StoneVO;

import org.osmf.layout.PaddingLayoutMetadata;

public class MatchState extends EventDispatcher{
	private static var _instance:MatchState;

	private var _started:Boolean;
	private var _stones:Vector.<StoneVO>;
	private var _clicks:Vector.<Point>;
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
		_clicks = new Vector.<Point>();
		UserState.instance.addEventListener(Event.CHANGE, onStateUpdate);
	}

	public function getLastStone():StoneVO {
		if (_stones && _stones.length > 0) {
			return _stones[_stones.length-1];
		}
		return null;
	}

	public function getLastClick():Point {
		if (_clicks && _clicks.length > 0) {
			return _clicks[_clicks.length-1];
		}
		return null;
	}

	public function get stones():Vector.<StoneVO> { return _stones; }
	public function get clicks():Vector.<Point> { return _clicks; }
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
			updateClicks(game["clicks"]);

			dispatchEvent(new MatchStateEvent(MatchStateEvent.UPDATED));

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
				var prevPhase:String = _phase;
				_phase = phase;
				if (prevPhase == BASIC_PHASE && phase == MAIN_PHASE) {
					_stones = new Vector.<StoneVO>();
					dispatchEvent(new MatchStateEvent(MatchStateEvent.BASIC_PHASE_CHANGED_ON_MAIN_PHASE));
				}
				dispatchEvent(new MatchStateEvent(MatchStateEvent.PHASE_CHANGED));
			}
		}
	}

	private function getUnclicks(clicks:Array):Vector.<Point> {
		var unclicks:Vector.<Point> = new Vector.<Point>();
		var founded:Boolean;
		for each (var point:Point in _clicks) {
			founded = false;
			for each (var clickObject:Object in clicks) {
				if (clickObject["x"] == point.x && clickObject["y"] == point.y) {
					founded = true;
				}
			}
			if (!founded) {
				unclicks.push(point);
			}
		}
		return unclicks;
	}

	private function updateClicks(clicks:Array):void {
		var unclicks:Vector.<Point> = getUnclicks(clicks);
		var index:int;
		for each (var point:Point in unclicks) {
			index = _clicks.indexOf(point);
			if (index != -1) {
				_clicks.splice(index, 1);
				dispatchEvent(new MatchStateClickEvent(MatchStateClickEvent.UNCLICK, point.x, point.y));
			}
		}

		for (var i:int = 0; i < clicks.length; ++i) {
			if (!clickExists(clicks[i]["x"], clicks[i]["y"])) {
				_clicks.push(new Point(clicks[i]["x"], clicks[i]["y"]));
				dispatchEvent(new MatchStateClickEvent(MatchStateClickEvent.CLICK, clicks[i]["x"], clicks[i]["y"]));
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
		for (var i:int = 0; i < stones.length; ++i) {
			if (!stoneExists(stones[i]["x"], stones[i]["y"], stones[i]["number"])) {
				if (!result) { result = new Vector.<StoneVO>(); }
				result.push(StoneVO.createStoneByObject(stones[i]));
			}
		}
		return result;
	}

	private function stoneExists(x:int, y:int, number:int):Boolean {
		for each (var stoneVO:StoneVO in _stones) {
			if (stoneVO.x == x && stoneVO.y == y && stoneVO.number == number) {
				return true;
			}
		}
		return false;
	}

	private function clickExists(x:int, y:int):Boolean {
		for each (var point:Point in _clicks) {
			if (point.x == x && point.y == y) {
				return true;
			}
		}
		return false;
	}

	private function addStone(stoneVO:StoneVO):void {
		_stones.push(stoneVO);
		dispatchEvent(new MatchStateEvent(MatchStateEvent.NEW_STONE));
		trace("new stone [MatchManager.addStone]");
	}

}
}

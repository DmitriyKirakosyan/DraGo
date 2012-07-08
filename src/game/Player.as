/**
 * Created by : Dmitry
 * Date: 4/23/12
 * Time: 8:56 PM
 */
package game {
import flash.events.EventDispatcher;

import game.events.BoardViewEvent;
import game.events.MatchStateEvent;
import game.events.PlayerMoveEvent;
import game.staticModel.MatchState;
import game.player.PlayerVO;
import game.staticModel.UserState;
import game.stone.StoneVO;

public class Player extends EventDispatcher {
	private var _boardView:BoardView;
	private var _vo:PlayerVO;
	private var _home:Boolean;
	private var _numHiddenStones:int;

	public function Player(vo:PlayerVO) {
		super();
		_vo = vo ? vo : new PlayerVO("", 0);
		_home = !vo || vo.userId == UserState.instance.userId;
		_numHiddenStones = 1;
		addListeners();
	}

	public function remove():void {
		if (_boardView) {
			_boardView.removeEventListener(BoardViewEvent.CLICK, onBoardViewClick);
		}
	}

	public function get vo():PlayerVO { return _vo; }
	public function get home():Boolean { return _home; }

	public function setBoardView(boardView:BoardView):void {
		if (_boardView) {
			trace("WARN! board view already exists [Player.setBoardView]");
			return;
		}
		_boardView = boardView;
		_boardView.addEventListener(BoardViewEvent.CLICK, onBoardViewClick);
	}

	/* Internal functions */

	private function addListeners():void {
		if (!_home) {
			MatchState.instance.addEventListener(MatchStateEvent.NEW_STONE, onNewStone);
		}
	}

	//remote player event
	private function onNewStone(event:MatchStateEvent):void {
		var stoneVO:StoneVO = MatchState.instance.getLastStone();
		if (stoneVO.color == _vo.color) {
			dispatchEvent(new PlayerMoveEvent(PlayerMoveEvent.MOVE, stoneVO));
		} else {
			trace("new stone not of this player [Player.onNewStone]");
		}
	}

	private function onBoardViewClick(event:BoardViewEvent):void {
		var hidden:Boolean = false;
		if (_numHiddenStones > 0 && event.shiftKey) {
			_numHiddenStones--;
			hidden = true;
		}
		dispatchEvent(new PlayerMoveEvent(PlayerMoveEvent.MOVE, new StoneVO(_vo.color, event.cellX, event.cellY, hidden)));
	}
}
}

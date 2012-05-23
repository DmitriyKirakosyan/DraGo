/**
 * Created by : Dmitry
 * Date: 4/23/12
 * Time: 8:56 PM
 */
package game {
import flash.events.EventDispatcher;

import game.events.BoardViewEvent;
import game.events.MatchStateEvent;
import game.events.PlayerEvent;
import game.staticModel.MatchState;
import game.player.PlayerVO;
import game.staticModel.UserState;
import game.stone.StoneVO;

import rpc.GameRpc;

public class Player extends EventDispatcher {
	private var _boardView:BoardView;
	private var _vo:PlayerVO;
	private var _home:Boolean;

	public function Player(vo:PlayerVO) {
		super();
		_vo = vo;
		_home = vo.userId == UserState.instance.userId;
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

	private function onNewStone(event:MatchStateEvent):void {
		var stoneVO:StoneVO = MatchState.instance.getLastStone();
		if (stoneVO.color == _vo.color) {
			dispatchEvent(new PlayerEvent(PlayerEvent.MOVE, stoneVO.x, stoneVO.y));
		}
	}

	private function onBoardViewClick(event:BoardViewEvent):void {
		dispatchEvent(new PlayerEvent(PlayerEvent.MOVE, event.cellX, event.cellY));
	}
}
}

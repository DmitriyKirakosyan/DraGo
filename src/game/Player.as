/**
 * Created by : Dmitry
 * Date: 4/23/12
 * Time: 8:56 PM
 */
package game {
import flash.events.EventDispatcher;

import game.events.BoardViewEvent;
import game.events.PlayerEvent;

public class Player extends EventDispatcher {
	private var _home:Boolean;
	private var _boardView:BoardView;

	public function Player(home:Boolean) {
		super();
		_home = home;
		addListeners();
	}

	public function remove():void {
		if (_boardView) {
			_boardView.removeEventListener(BoardViewEvent.CLICK, onBoardViewClick);
		}
	}

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
		if (_home) {

		}
	}

	private function onBoardViewClick(event:BoardViewEvent):void {
		dispatchEvent(new PlayerEvent(PlayerEvent.MOVE, event.cellX, event.cellY));
	}
}
}

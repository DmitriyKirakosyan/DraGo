/**
 * Created by IntelliJ IDEA.
 * User: dima
 * Date: 6/13/12
 * Time: 2:35 AM
 * To change this template use File | Settings | File Templates.
 */
package game.iface.window {
import flash.display.Sprite;

import game.events.MatchStateEvent;

import game.iface.window.panel.GamePlayersPanel;
import game.staticModel.MatchState;
import game.stone.StoneVO;

public class GameInterface extends Sprite {
	private var _container:Sprite;
	private var _playersPanel:GamePlayersPanel;

	public function GameInterface(container:Sprite) {
		super();
		_container = container;
		init();
		MatchState.instance.addEventListener(MatchStateEvent.PHASE_CHANGED, onPhaseChanged);
	}

	public function open():void {
		_container.addChild(_playersPanel);
	}

	public function close():void {
		if (_container.contains(_playersPanel)) {
			_container.removeChild(_playersPanel);
		}
	}

	private function init():void {
		_playersPanel = new GamePlayersPanel();
		_playersPanel.x = 31;
		_playersPanel.y = 3;
	}

	private function onPhaseChanged(event:MatchStateEvent):void {
		MatchState.instance.removeEventListener(MatchStateEvent.CHANGE_MOVE_PLAYER, onChangeMovePlayer);
		_playersPanel.setNothingMove();
		if (MatchState.instance.phase == MatchState.MAIN_PHASE) {
			_playersPanel.changeMove(MatchState.instance.getMovePlayerColor());
			MatchState.instance.addEventListener(MatchStateEvent.CHANGE_MOVE_PLAYER, onChangeMovePlayer);
		}
	}

	private function onChangeMovePlayer(event:MatchStateEvent):void {
		_playersPanel.changeMove(MatchState.instance.getMovePlayerColor());
	}

}
}
